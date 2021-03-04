-- --- === hs.host.sysctl.conversions ===
-- ---
-- --- Data conversion helpers for hs.host.sysctl
-- ---
-- --- Contains helper functions used internally by hs.host.sysctl to convert
-- --- raw binary blobs into something useful for the end user. Functions
-- --- within this sub-module are not intended to be used directly by the user
-- --- but for convenience they are collected here in one place.
-- ---
-- --- The intention is to handle these entirely lua side. Should that end up
-- --- being unrealistic, the support file should be named
-- --- `conversions_internal.m` to simplify module file organization within
-- --- Hammerspoon.

-- Maybe put in test suite we can ask users to run to find out if other system configurations
-- can answer questions about structs we can't test:

    -- struct if_family_id {
    --     u_int32_t               iffmid_len;
    --     u_int32_t               iffmid_id;
    --     char                    iffmid_str[1];  /* variable length string */
    -- };
    -- 0 length return on M1 Big Sur, Intel Big Sur, and Intel Catalina, so can't test
    --     unsure if iffmid_str is \0 terminated or iffmid_len bytes, so waiting until
    --     requested


local USERDATA_TAG = "hs.host.sysctl.conversions"
-- local module       = require(USERDATA_TAG.."_internal")
local module = {}

local inspect = require("hs.inspect")

-- don't load sysctl module yet -- it would cause a loop
local sysctl    = nil
local typeSizes = nil

-- alows sysctl to load us with `local conversions = require("hs.host.sysctl.conversions").init(module)`
module.init = function(parent)
    sysctl = parent
    typeSizes = parent.typeSizes
    module.init = nil -- no reason to stick around anymore
    return module
end

-- and just to be safe in case we're loaded directly (to add experimental conversions, for example)
local loopPreventionTimer
loopPreventionTimer = require("hs.timer").doAfter(require("hs.math").minFloat, function()
    sysctl = package.loaded["hs.host.sysctl"]
    if not sysctl then -- don't have it, so force load
        -- will call our init function to set vars
        require("hs.host.sysctl")
        loopPreventionTimer = nil -- makes this an upvalue-captured local
    end
end)
-- Note that this won't run until there has been an idle slice in Hammerspoon, so something like
-- the following without a break first (coroutine.applicationYield, completion of users's initial
-- init.lua so console *can* be displayed (doesn't have to be, though), etc)  will fail (assuming
-- hs.host.sysctl hasn't been loaded elsewhere first, that is).
--
-- The proper fix is load modules in the correct order!
--
-- local conversions = require("hs.host.sysctl.conversions")
-- conversions.S_quads(testData)
--
-- To fix, do `require("hs.host.sysctl")` *before* loading hs.host.sysctl.conversions.

local flatInspect = function(self)
    return inspect(self, { newline = " ", indent = "" })
end

-- settings with periods in them can't be watched via KVO with hs.settings.watchKey, so
-- in general it's a good idea not to include periods
-- local SETTINGS_TAG = USERDATA_TAG:gsub("%.", "_")
-- local settings     = require("hs.settings")
-- local log          = require("hs.logger").new(USERDATA_TAG, settings.get(SETTINGS_TAG .. "_logLevel") or "warning")

-- private variables and methods -----------------------------------------
-- Prior to 64bit processors, 64-bit integers were indicated as opaque/structure with
-- a fmt string of 'Q'. Some legacy entities may still do this.
local S_quads = function(data)
    return module.genericInt(typeSizes.Q, data)
end

-- Prior to 64bit processors, 64-bit unsigned integers were indicated as opaque/structure
-- with a fmt string of 'QU'. Some legacy entities may still do this.
local S_unsignedQuads = function(data)
    return module.genericInt(typeSizes.QU, data)
end

-- Public interface ------------------------------------------------------

-- As defined in sys/time.h as:
-- struct clockinfo {
--     int     hz;             /* clock frequency */
--     int     tick;           /* micro-seconds per hz tick */
--     int     tickadj;        /* clock skew rate for adjtime() */
--     int     stathz;         /* statistics clock frequency */
--     int     profhz;         /* profiling clock frequency */
-- };
module.S_clockinfo = function(data)
    if #data < 20 then
        return string.format("!! S_clockinfo size of %d != 20", #data)
    else
        local structFormat = typeSizes.int ..
                             typeSizes.int ..
                             typeSizes.int ..
                             typeSizes.int ..
                             typeSizes.int
        local clockinfo = { string.unpack(structFormat, data) }
        table.remove(clockinfo) -- remove position from string.unpack results

        local results = {
            hz       = clockinfo[1],
            tick     = clockinfo[2],
            tickaadj = clockinfo[3],
            profhz   = clockinfo[4],
            staths   = clockinfo[5],
        }

        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in sys/_types/_timeval.h as:
-- _STRUCT_TIMEVAL
-- {
--     __darwin_time_t         tv_sec;         /* seconds */
--     __darwin_suseconds_t    tv_usec;        /* and microseconds */
-- };
-- where both __darwin_time_t and are __darwin_suseconds_t long (why not unsigned?)
module.S_timeval = function(data)
    if #data < 16 then
        return string.format("!! S_timeval size of %d != 16", #data)
    else
        local structFormat = typeSizes.long ..
                             typeSizes.long
        local timeval = { string.unpack(structFormat, data) }
        table.remove(timeval) -- remove position from string.unpack results

        local results = {
            secs = timeval[1],
            usec = timeval[2],
            user = os.date("%c", timeval[1]),
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in sys/sysctl.h
-- struct loadavg {
--     fixpt_t ldavg[3]; <-- shouldn't this be 4?
--     long    fscale;
-- };
-- where fixpt_t is u_int_32
--
-- actually, not sure *why* this works because C array's start at 0 and end at 1 minus length,
-- but a look at the raw data returned *does* show 24 bytes and not 20... padding? If so,
-- sys/sysctl.h is making a *huge* assumption about the compiler and architecture...
module.S_loadavg = function(data)
    if #data < 24 then
        return string.format("!! S_loadavg size of %d != 16", #data)
    else
        local structFormat = typeSizes.fixpt_t .. -- ldavg[0]
                             typeSizes.fixpt_t .. -- ldavg[1]
                             typeSizes.fixpt_t .. -- ldavg[2]
                             typeSizes.fixpt_t .. -- padding?
                             typeSizes.long       -- fscale
        local loadavg = { string.unpack(structFormat, data) }
        table.remove(loadavg) -- remove position from string.unpack results

        local results = {
            loadavg[1] / loadavg[5], loadavg[2] / loadavg[5], loadavg[3] / loadavg[5]
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in sys/sysctl.h
-- struct xsw_usage {
--     u_int64_t       xsu_total;
--     u_int64_t       xsu_avail;
--     u_int64_t       xsu_used;
--     u_int32_t       xsu_pagesize;
--     boolean_t       xsu_encrypted;
-- };
-- where boolean_t is int
module.S_xsw_usage = function(data)
    if #data < 32 then
        return string.format("!! S_loadavg size of %d != 32", #data)
    else
        local structFormat = typeSizes.u_int64_t ..
                             typeSizes.u_int64_t ..
                             typeSizes.u_int64_t ..
                             typeSizes.u_int32_t ..
                             typeSizes.boolean_t
        local xsw_usage = { string.unpack(structFormat, data) }
        table.remove(xsw_usage) -- remove position from string.unpack results

        local results = {
            total     = xsw_usage[1] / (1024.0 * 1024.0),
            used      = xsw_usage[3] / (1024.0 * 1024.0),
            free      = xsw_usage[2] / (1024.0 * 1024.0),
            encrypted = (xsw_usage[5] ~= 0),
            pagesize  = xsw_usage[4],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- Haven't seen on Big Sur M1, Big Sur Intel, or Catalina Intel, so not certain, but best
--     guess from sys/_types/_dev_t.h and sys/_types.h is that this is a 32 bit int.
-- Is this a deprecated type?
module.T_dev_t = function(data)
    if #data < 4 then
        return string.format("!! T_dev_t size of %d != 4", #data)
    else
        local d = string.unpack(typeSizes.int32_t, data)
        local major_d = (d >> 24) & 0xff
        local minor_d = d & 0xffffff

        local results = {
            major = major_d,
            minor = (minor_d > 255 or minor_d < 0) and string.format("0x%x", minor_d) or minor_d
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in net/if_arp.h:
module.S_arpstat = function(data)
    if #data < 64 then
        return string.format("!! S_arpstat size of %d != 64", #data)
    else
        local structFormat = typeSizes.u_int32_t .. -- txrequests;    /* # of ARP requests sent by this host. */
                             typeSizes.u_int32_t .. -- txreplies;     /* # of ARP replies sent by this host. */
                             typeSizes.u_int32_t .. -- txannounces;   /* # of ARP announcements sent by this host. */
                             typeSizes.u_int32_t .. -- rxrequests;    /* # of ARP requests received by this host. */
                             typeSizes.u_int32_t .. -- rxreplies;     /* # of ARP replies received by this host. */
                             typeSizes.u_int32_t .. -- received;      /* # of ARP packets received by this host. */
                             typeSizes.u_int32_t .. -- txconflicts;   /* # of ARP conflict probes sent */
                             typeSizes.u_int32_t .. -- invalidreqs;   /* # of invalid ARP resolve requests */
                             typeSizes.u_int32_t .. -- reqnobufs;     /* # of failed requests due to no memory */
                             typeSizes.u_int32_t .. -- dropped;       /* # of packets dropped waiting for a reply. */
                             typeSizes.u_int32_t .. -- purged;        /* # of packets purged while removing entries */
                             typeSizes.u_int32_t .. -- timeouts;      /* # of times with entries removed due to timeout. */
                             typeSizes.u_int32_t .. -- dupips;        /* # of duplicate IPs detected. */
                             typeSizes.u_int32_t .. -- inuse;         /* # of ARP entries in routing table */
                             typeSizes.u_int32_t .. -- txurequests;   /* # of ARP requests sent (unicast) */
                             typeSizes.u_int32_t    -- held;          /* # of packets held waiting for a reply */

        local arpstat = { string.unpack(structFormat, data) }
        table.remove(arpstat) -- remove position from string.unpack results

        local results = {
            txrequests  = arpstat[1],
            txreplies   = arpstat[2],
            txannounces = arpstat[3],
            rxrequests  = arpstat[4],
            rxreplies   = arpstat[5],
            received    = arpstat[6],
            txconflicts = arpstat[7],
            invalidreqs = arpstat[8],
            reqnobufs   = arpstat[9],
            dropped     = arpstat[10],
            purged      = arpstat[11],
            timeouts    = arpstat[12],
            dupips      = arpstat[13],
            inuse       = arpstat[14],
            txurequests = arpstat[15],
            held        = arpstat[16],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in netinet/udp_var.h:
module.S_udpstat = function(data)
    if #data < 88 then
        return string.format("!! S_udpstat size of %d != 88", #data)
    else
        local structFormat = typeSizes.u_int32_t .. -- udps_ipackets;        /* total input packets */
                             typeSizes.u_int32_t .. -- udps_hdrops;          /* packet shorter than header */
                             typeSizes.u_int32_t .. -- udps_badsum;          /* checksum error */
                             typeSizes.u_int32_t .. -- udps_badlen;          /* data length larger than packet */
                             typeSizes.u_int32_t .. -- udps_noport;          /* no socket on port */
                             typeSizes.u_int32_t .. -- udps_noportbcast;     /* of above, arrived as broadcast */
                             typeSizes.u_int32_t .. -- udps_fullsock;        /* not delivered, input socket full */
                             typeSizes.u_int32_t .. -- udpps_pcbcachemiss;   /* input packets missing pcb cache */
                             typeSizes.u_int32_t .. -- udpps_pcbhashmiss;    /* input packets not for hashed pcb */
                             typeSizes.u_int32_t .. -- udps_opackets;        /* total output packets */
                             typeSizes.u_int32_t .. -- udps_fastout;         /* output packets on fast path */
                             typeSizes.u_int32_t .. -- udps_nosum;           /* no checksum */
                             typeSizes.u_int32_t .. -- udps_noportmcast;     /* of no socket on port, multicast */
                             typeSizes.u_int32_t .. -- udps_filtermcast;     /* blocked by multicast filter */
                             typeSizes.u_int32_t .. -- udps_rcv_swcsum;        /* udp swcksum (inbound), packets */
                             typeSizes.u_int32_t .. -- udps_rcv_swcsum_bytes;  /* udp swcksum (inbound), bytes */
                             typeSizes.u_int32_t .. -- udps_rcv6_swcsum;       /* udp6 swcksum (inbound), packets */
                             typeSizes.u_int32_t .. -- udps_rcv6_swcsum_bytes; /* udp6 swcksum (inbound), bytes */
                             typeSizes.u_int32_t .. -- udps_snd_swcsum;        /* udp swcksum (outbound), packets */
                             typeSizes.u_int32_t .. -- udps_snd_swcsum_bytes;  /* udp swcksum (outbound), bytes */
                             typeSizes.u_int32_t .. -- udps_snd6_swcsum;       /* udp6 swcksum (outbound), packets */
                             typeSizes.u_int32_t    -- udps_snd6_swcsum_bytes; /* udp6 swcksum (outbound), bytes */

        local udpstat = { string.unpack(structFormat, data) }
        table.remove(udpstat) -- remove position from string.unpack results

        local results = {
            ipackets          = udpstat[1],
            hdrops            = udpstat[2],
            badsum            = udpstat[3],
            badlen            = udpstat[4],
            noport            = udpstat[5],
            noportbcast       = udpstat[6],
            fullsock          = udpstat[7],
            pcbcachemiss      = udpstat[8],
            pcbhashmiss       = udpstat[9],
            opackets          = udpstat[10],
            fastout           = udpstat[11],
            nosum             = udpstat[12],
            noportmcast       = udpstat[13],
            filtermcast       = udpstat[14],
            rcv_swcsum        = udpstat[15],
            rcv_swcsum_bytes  = udpstat[16],
            rcv6_swcsum       = udpstat[17],
            rcv6_swcsum_bytes = udpstat[18],
            snd_swcsum        = udpstat[19],
            snd_swcsum_bytes  = udpstat[20],
            snd6_swcsum       = udpstat[21],
            snd6_swcsum_bytes = udpstat[22],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in netinet/ip_var.h
module.S_ipstat = function(data)
    if #data < 184 then
        return string.format("!! S_ipstat size of %d != 184", #data)
    else
        local structFormat = typeSizes.u_int32_t  .. -- total;             /* total packets received */
                             typeSizes.u_int32_t  .. -- badsum;            /* checksum bad */
                             typeSizes.u_int32_t  .. -- tooshort;          /* packet too short */
                             typeSizes.u_int32_t  .. -- toosmall;          /* not enough data */
                             typeSizes.u_int32_t  .. -- badhlen;           /* ip header length < data size */
                             typeSizes.u_int32_t  .. -- badlen;            /* ip length < ip header length */
                             typeSizes.u_int32_t  .. -- fragments;         /* fragments received */
                             typeSizes.u_int32_t  .. -- fragdropped;       /* frags dropped (dups, out of space) */
                             typeSizes.u_int32_t  .. -- fragtimeout;       /* fragments timed out */
                             typeSizes.u_int32_t  .. -- forward;           /* packets forwarded */
                             typeSizes.u_int32_t  .. -- fastforward;       /* packets fast forwarded */
                             typeSizes.u_int32_t  .. -- cantforward;       /* packets rcvd for unreachable dest */
                             typeSizes.u_int32_t  .. -- redirectsent;      /* packets forwarded on same net */
                             typeSizes.u_int32_t  .. -- noproto;           /* unknown or unsupported protocol */
                             typeSizes.u_int32_t  .. -- delivered;         /* datagrams delivered to upper level */
                             typeSizes.u_int32_t  .. -- localout;          /* total ip packets generated here */
                             typeSizes.u_int32_t  .. -- odropped;          /* lost packets due to nobufs, etc. */
                             typeSizes.u_int32_t  .. -- reassembled;       /* total packets reassembled ok */
                             typeSizes.u_int32_t  .. -- fragmented;        /* datagrams successfully fragmented */
                             typeSizes.u_int32_t  .. -- ofragments;        /* output fragments created */
                             typeSizes.u_int32_t  .. -- cantfrag;          /* don't fragment flag was set, etc. */
                             typeSizes.u_int32_t  .. -- badoptions;        /* error in option processing */
                             typeSizes.u_int32_t  .. -- noroute;           /* packets discarded due to no route */
                             typeSizes.u_int32_t  .. -- badvers;           /* ip version != 4 */
                             typeSizes.u_int32_t  .. -- rawout;            /* total raw ip packets generated */
                             typeSizes.u_int32_t  .. -- toolong;           /* ip length > max ip packet size */
                             typeSizes.u_int32_t  .. -- notmember;         /* multicasts for unregistered grps */
                             typeSizes.u_int32_t  .. -- nogif;             /* no match gif found */
                             typeSizes.u_int32_t  .. -- badaddr;           /* invalid address on header */
                             typeSizes.u_int32_t  .. -- pktdropcntrl;      /* pkt dropped, no mbufs for ctl data */
                             typeSizes.u_int32_t  .. -- rcv_swcsum;        /* ip hdr swcksum (inbound), packets */
                             typeSizes.u_int32_t  .. -- rcv_swcsum_bytes;  /* ip hdr swcksum (inbound), bytes */
                             typeSizes.u_int32_t  .. -- snd_swcsum;        /* ip hdr swcksum (outbound), packets */
                             typeSizes.u_int32_t  .. -- snd_swcsum_bytes;  /* ip hdr swcksum (outbound), bytes */
                             typeSizes.u_int32_t  .. -- adj;               /* total packets trimmed/adjusted */
                             typeSizes.u_int32_t  .. -- adj_hwcsum_clr;    /* hwcksum discarded during adj */
                             typeSizes.u_int32_t  .. -- rxc_collisions;    /* rx chaining collisions */
                             typeSizes.u_int32_t  .. -- rxc_chained;       /* rx chains */
                             typeSizes.u_int32_t  .. -- rxc_notchain;      /* rx bypassed chaining */
                             typeSizes.u_int32_t  .. -- rxc_chainsz_gt2;   /* rx chain size greater than 2 */
                             typeSizes.u_int32_t  .. -- rxc_chainsz_gt4;   /* rx chain size greater than 4 */
                             typeSizes.u_int32_t  .. -- rxc_notlist;       /* count of pkts through ip_input */
                             typeSizes.u_int32_t  .. -- raw_sappend_fail;  /* sock append failed */
                             typeSizes.u_int32_t  .. -- necp_policy_drop;  /* NECP policy related drop */
                             typeSizes.u_int32_t  .. -- rcv_if_weak_match; /* packets whose receive interface that passed the Weak ES address check */
                             typeSizes.u_int32_t     -- rcv_if_no_match;   /* packets whose receive interface did not pass the address check */

        local ipstat = { string.unpack(structFormat, data) }
        table.remove(ipstat) -- remove position from string.unpack results

        local results = {
            total              = ipstat[1],
            badsum             = ipstat[2],
            tooshort           = ipstat[3],
            toosmall           = ipstat[4],
            badhlen            = ipstat[5],
            badlen             = ipstat[6],
            fragments          = ipstat[7],
            fragdropped        = ipstat[8],
            fragtimeout        = ipstat[9],
            forward            = ipstat[10],
            fastforward        = ipstat[11],
            cantforward        = ipstat[12],
            redirectsent       = ipstat[13],
            noproto            = ipstat[14],
            delivered          = ipstat[15],
            localout           = ipstat[16],
            odropped           = ipstat[17],
            reassembled        = ipstat[18],
            fragmented         = ipstat[19],
            ofragments         = ipstat[20],
            cantfrag           = ipstat[21],
            badoptions         = ipstat[22],
            noroute            = ipstat[23],
            badvers            = ipstat[24],
            rawout             = ipstat[25],
            toolong            = ipstat[26],
            notmember          = ipstat[27],
            nogif              = ipstat[28],
            badaddr            = ipstat[29],
            pktdropcntrl       = ipstat[30],
            rcv_swcsum         = ipstat[31],
            rcv_swcsum_bytes   = ipstat[32],
            snd_swcsum         = ipstat[33],
            snd_swcsum_bytes   = ipstat[34],
            adj                = ipstat[35],
            adj_hwcsum_clr     = ipstat[36],
            rxc_collisions     = ipstat[37],
            rxc_chained        = ipstat[38],
            rxc_notchain       = ipstat[39],
            rxc_chainsz_gt2    = ipstat[40],
            rxc_chainsz_gt4    = ipstat[41],
            rxc_notlist        = ipstat[42],
            raw_sappend_fail   = ipstat[43],
            necp_policy_drop   = ipstat[44],
            rcv_if_weak_match  = ipstat[45],
            rcv_if_no_match    = ipstat[46],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in netinet/ip_var.h
module.S_ip_linklocal_stat = function(data)
    if #data < 16 then
        return string.format("!! S_ip_linklocal_stat size of %d != 16", #data)
    else
        local structFormat = typeSizes.u_int32_t .. --      iplls_in_total;
                             typeSizes.u_int32_t .. --      iplls_in_badttl;
                             typeSizes.u_int32_t .. --      iplls_out_total;
                             typeSizes.u_int32_t    --      iplls_out_badttl;

        local ip_linklocal_stat = { string.unpack(structFormat, data) }
        table.remove(ip_linklocal_stat) -- remove position from string.unpack results

        local results = {
            in_total   = ip_linklocal_stat[1],
            in_badttl  = ip_linklocal_stat[2],
            out_total  = ip_linklocal_stat[3],
            out_badttl = ip_linklocal_stat[4],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in netinet/icmp_var.h and netinet/ip_icmp.h
module.S_icmpstat = function(data)
    if #data < 368 then
        return string.format("!! S_icmpstat size of %d != 368", #data)
    else
        local structFormat = typeSizes.u_int32_t ..                 -- icps_error;     /* # of calls to icmp_error */
                             typeSizes.u_int32_t ..                 -- icps_oldshort;  /* no error 'cuz old ip too short */
                             typeSizes.u_int32_t ..                 -- icps_oldicmp;   /* no error 'cuz old was icmp */
                             string.rep(typeSizes.u_int32_t, 41) .. -- icps_outhist[ICMP_MAXTYPE + 1];
                             typeSizes.u_int32_t ..                 -- icps_badcode;   /* icmp_code out of range */
                             typeSizes.u_int32_t ..                 -- icps_tooshort;  /* packet < ICMP_MINLEN */
                             typeSizes.u_int32_t ..                 -- icps_checksum;  /* bad checksum */
                             typeSizes.u_int32_t ..                 -- icps_badlen;    /* calculated bound mismatch */
                             typeSizes.u_int32_t ..                 -- icps_reflect;   /* number of responses */
                             string.rep(typeSizes.u_int32_t, 41) .. -- icps_inhist[ICMP_MAXTYPE + 1];
                             typeSizes.u_int32_t ..                 -- icps_bmcastecho;/* b/mcast echo requests dropped */
                             typeSizes.u_int32_t                    -- icps_bmcasttstamp; /* b/mcast tstamp requests dropped */

        local icmpstat = { string.unpack(structFormat, data) }
        table.remove(icmpstat) -- remove position from string.unpack results

        local results = {
            error        = icmpstat[1],
            oldshort     = icmpstat[2],
            oldicmp      = icmpstat[3],
            outhist      = {
                echoReply                 = icmpstat[4 + 0],
                destinationUnreachable    = icmpstat[4 + 3],
                sourceQuench              = icmpstat[4 + 4],
                redirect                  = icmpstat[4 + 5],
                alternateHostAddress      = icmpstat[4 + 6],
                echo                      = icmpstat[4 + 8],
                routerAdvertisement       = icmpstat[4 + 9],
                routerSolicitation        = icmpstat[4 + 10],
                timeExceeded              = icmpstat[4 + 11],
                parameterProblem          = icmpstat[4 + 12],
                timestampRequest          = icmpstat[4 + 13],
                timestampReply            = icmpstat[4 + 14],
                informationRequest        = icmpstat[4 + 15],
                informationReply          = icmpstat[4 + 16],
                addressMaskRequest        = icmpstat[4 + 17],
                addressMaskReply          = icmpstat[4 + 18],
                traceroute                = icmpstat[4 + 30],
                dataConversionError       = icmpstat[4 + 31],
                mobileHostRedirect        = icmpstat[4 + 32],
                ipv6_WhereAreYou          = icmpstat[4 + 33],
                ipv6_IAmHere              = icmpstat[4 + 34],
                mobileRegistrationRequest = icmpstat[4 + 35],
                mobileRegistrationReply   = icmpstat[4 + 36],
                skip                      = icmpstat[4 + 39],
                photuris                  = icmpstat[4 + 40],
            },
            badcode      = icmpstat[45],
            tooshort     = icmpstat[46],
            checksum     = icmpstat[47],
            badlen       = icmpstat[48],
            reflect      = icmpstat[49],
            inhist       = {
                echoReply                 = icmpstat[50 + 0],
                destinationUnreachable    = icmpstat[50 + 3],
                sourceQuench              = icmpstat[50 + 4],
                redirect                  = icmpstat[50 + 5],
                alternateHostAddress      = icmpstat[50 + 6],
                echo                      = icmpstat[50 + 8],
                routerAdvertisement       = icmpstat[50 + 9],
                routerSolicitation        = icmpstat[50 + 10],
                timeExceeded              = icmpstat[50 + 11],
                parameterProblem          = icmpstat[50 + 12],
                timestampRequest          = icmpstat[50 + 13],
                timestampReply            = icmpstat[50 + 14],
                informationRequest        = icmpstat[50 + 15],
                informationReply          = icmpstat[50 + 16],
                addressMaskRequest        = icmpstat[50 + 17],
                addressMaskReply          = icmpstat[50 + 18],
                traceroute                = icmpstat[50 + 30],
                dataConversionError       = icmpstat[50 + 31],
                mobileHostRedirect        = icmpstat[50 + 32],
                ipv6_WhereAreYou          = icmpstat[50 + 33],
                ipv6_IAmHere              = icmpstat[50 + 34],
                mobileRegistrationRequest = icmpstat[50 + 35],
                mobileRegistrationReply   = icmpstat[50 + 36],
                skip                      = icmpstat[50 + 39],
                photuris                  = icmpstat[50 + 40],
            },
            bmcastecho   = icmpstat[91],
            bmcasttstamp = icmpstat[21],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in netinet/igmp_var.h
module.S_igmpstat = function(data)
    if #data < 36 then
        return string.format("!! S_igmpstat size of %d != 36", #data)
    else
        local structFormat = typeSizes.unsignedInt .. --   igps_rcv_total;         /* total IGMP messages received */
                             typeSizes.unsignedInt .. --   igps_rcv_tooshort;      /* received with too few bytes */
                             typeSizes.unsignedInt .. --   igps_rcv_badsum;        /* received with bad checksum */
                             typeSizes.unsignedInt .. --   igps_rcv_queries;       /* received membership queries */
                             typeSizes.unsignedInt .. --   igps_rcv_badqueries;    /* received invalid queries */
                             typeSizes.unsignedInt .. --   igps_rcv_reports;       /* received membership reports */
                             typeSizes.unsignedInt .. --   igps_rcv_badreports;    /* received invalid reports */
                             typeSizes.unsignedInt .. --   igps_rcv_ourreports;    /* received reports for our groups */
                             typeSizes.unsignedInt    --   igps_snd_reports;       /* sent membership reports */

        local igmpstat = { string.unpack(structFormat, data) }
        table.remove(igmpstat) -- remove position from string.unpack results

        local results = {
            total       = igmpstat[1],
            tooshort    = igmpstat[2],
            badsum      = igmpstat[3],
            queries     = igmpstat[4],
            badqueries  = igmpstat[5],
            reports     = igmpstat[6],
            badreports  = igmpstat[7],
            ourreports  = igmpstat[8],
            sentreports = igmpstat[9],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in netinet/igmp_var.h
module.S_igmpstat_v3 = function(data)
    if #data < 168 then
        return string.format("!! S_igmpstat_v3 size of %d != 168", #data)
    else
        local structFormat = typeSizes.u_int32_t .. -- igps_version;          /* version of this structure */
                             typeSizes.u_int32_t .. -- igps_len;              /* length of this structure */
                             typeSizes.u_int64_t .. -- igps_rcv_total;        /* total IGMP messages received */
                             typeSizes.u_int64_t .. -- igps_rcv_tooshort;     /* received with too few bytes */
                             typeSizes.u_int64_t .. -- igps_rcv_badttl;       /* received with ttl other than 1 */
                             typeSizes.u_int64_t .. -- igps_rcv_badsum;       /* received with bad checksum */
                             typeSizes.u_int64_t .. -- igps_rcv_v1v2_queries; /* received IGMPv1/IGMPv2 queries */
                             typeSizes.u_int64_t .. -- igps_rcv_v3_queries;   /* received IGMPv3 queries */
                             typeSizes.u_int64_t .. -- igps_rcv_badqueries;   /* received invalid queries */
                             typeSizes.u_int64_t .. -- igps_rcv_gen_queries;  /* received general queries */
                             typeSizes.u_int64_t .. -- igps_rcv_group_queries;/* received group queries */
                             typeSizes.u_int64_t .. -- igps_rcv_gsr_queries;  /* received group-source queries */
                             typeSizes.u_int64_t .. -- igps_drop_gsr_queries; /* dropped group-source queries */
                             typeSizes.u_int64_t .. -- igps_rcv_reports;      /* received membership reports */
                             typeSizes.u_int64_t .. -- igps_rcv_badreports;   /* received invalid reports */
                             typeSizes.u_int64_t .. -- igps_rcv_ourreports;   /* received reports for our groups */
                             typeSizes.u_int64_t .. -- igps_rcv_nora;         /* received w/o Router Alert option */
                             typeSizes.u_int64_t .. -- igps_snd_reports;      /* sent membership reports */
                             typeSizes.u_int64_t    -- __igps_pad[4];

        local igmpstat_v3 = { string.unpack(structFormat, data) }
        table.remove(igmpstat_v3) -- remove position from string.unpack results

        local results = {
            version           = igmpstat_v3[1],
            len               = igmpstat_v3[2],
            rcv_total         = igmpstat_v3[3],
            rcv_tooshort      = igmpstat_v3[4],
            rcv_badttl        = igmpstat_v3[5],
            rcv_badsum        = igmpstat_v3[6],
            rcv_v1v2_queries  = igmpstat_v3[7],
            rcv_v3_queries    = igmpstat_v3[8],
            rcv_badqueries    = igmpstat_v3[9],
            rcv_gen_queries   = igmpstat_v3[10],
            rcv_group_queries = igmpstat_v3[11],
            rcv_gsr_queries   = igmpstat_v3[12],
            drop_gsr_queries  = igmpstat_v3[13],
            rcv_reports       = igmpstat_v3[14],
            rcv_badreports    = igmpstat_v3[15],
            rcv_ourreports    = igmpstat_v3[16],
            rcv_nora          = igmpstat_v3[17],
            snd_reports       = igmpstat_v3[18],
        }
        local resultsAsString = flatInspect(results)
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- As defined in sys/_types/_uuid_t.h and sys/_types.h
module.S_uuid_t = function(data)
    if #data < 16 then
        return string.format("!! S_igmpstat_v3 size of %d != 16", #data)
    else
        local structFormat = string.rep(typeSizes.unsignedChar, 16)

        local uuid_t = { string.unpack(structFormat, data) }
        table.remove(uuid_t) -- remove position from string.unpack results


        local results = string.format(
            "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
            uuid_t[1], uuid_t[2], uuid_t[3], uuid_t[4],
            uuid_t[5], uuid_t[6],
            uuid_t[7], uuid_t[8],
            uuid_t[9], uuid_t[10],
            uuid_t[11], uuid_t[12], uuid_t[13], uuid_t[14], uuid_t[15], uuid_t[16]
        )
        return results
    end
end

module.quadMappings = {
    ["Q"]  = S_quads,
    ["QU"] = S_unsignedQuads, --
    ["UQ"] = S_unsignedQuads,
}

module.genericInt = function(pattern, data)
    local divider = tonumber(pattern:match("(%d+)$")) or 4
    local count = #data / divider
    local fmt = string.rep(pattern, count)
    local results = { string.unpack(fmt, data) }
    table.remove(results) -- remove position from string.unpack results

    local resultsAsString = flatInspect(results)
    if #results == 1 then
        return results[1]
    else
        return setmetatable(results, { __tostring = function(_) return resultsAsString end })
    end
end

-- Return Module Object --------------------------------------------------

return module
