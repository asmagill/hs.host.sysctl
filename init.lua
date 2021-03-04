--- === hs.host.sysctl ===
---
--- Interface for Hammerspoon to read kernel information provided via sysctl
---
--- This module is largely based on code found at:
---    * https://opensource.apple.com/source/system_cmds/system_cmds-880.60.2/sysctl.tproj/sysctl.c.auto.html
--- With additional insights from:
---    * https://opensource.apple.com/source/xnu/xnu-7195.81.3/bsd/kern/kern_sysctl.c.auto.html
---    * https://opensource.apple.com/source/xnu/xnu-7195.81.3/bsd/kern/kern_newsysctl.c.auto.html

local USERDATA_TAG = "hs.host.sysctl"
local module       = require(USERDATA_TAG..".internal")
local fnutils      = require("hs.fnutils")
local inspect      = require("hs.inspect")

local basePath = package.searchpath(USERDATA_TAG, package.path)
if basePath then
    basePath = basePath:match("^(.+)/init.lua$")
    if require"hs.fs".attributes(basePath .. "/docs.json") then
        require"hs.doc".registerJSONFile(basePath .. "/docs.json")
    end
end

-- pass us in so that the conversions can access our type tables
local conversions = require(USERDATA_TAG..".conversions").init(module)

-- private variables and methods -----------------------------------------

-- derived from `man 3 sysctl` and sysctl.tproj (see links above) and errors i've
-- seen during testing
local ERROR = {   -- from errno.h
    EFAULT     = 14,
    EINVAL     = 22,
    ENOMEM     = 12,
    ENOTDIR    = 20,
    EISDIR     = 21,
    ENOENT     = 2,
    EPERM      = 1,
    ENOTSUP    = 45,
    EOPNOTSUPP = 102,
    EIO        = 5,
    ENXIO      = 6,
    EACCES     = 13,
}

local ERROR_MSG = {
    [ERROR.EFAULT]     = "invalid address",
    [ERROR.EINVAL]     = "oid size mismatch",
    [ERROR.ENOMEM]     = "oid type unknown or memory allocation error",
    [ERROR.ENOTDIR]    = "incomplete oid specification",
    [ERROR.EISDIR]     = "oid extends beyond valid leaf",
    [ERROR.ENOENT]     = "unknown value",
    [ERROR.EPERM]      = "insufficient privileges",
    [ERROR.ENOTSUP]    = "value not available",
    [ERROR.EOPNOTSUPP] = "value not available",
    [ERROR.EIO]        = "input/output error",
    [ERROR.ENXIO]      = "device not configured",
    [ERROR.EACCES]     = "permission denied",
}

-- predeclare for use in sysctl and metatables
local makeOID

-- module.sysctl is wrapped here so that errno is always captured in module.errno
-- it is otherwise unchanged from the internal.m version, so it is documented there
local _core_sysctl = module.sysctl
module.sysctl = function(...)
    local oid = { ... }
    if #oid == 1 and (type(oid[1]) == "table" or type(oid[1]) == "string") then oid = oid[1] end

    local metatableName = (getmetatable(oid) or {}).__name
    if metatableName and metatableName:match("%.node") then
        oid = oid._oid
    elseif not (metatableName and metatableName:match("%.oid")) then
        oid = makeOID(oid)
    end

    module.errno = nil
    local result = { _core_sysctl(oid) }
    if type(result[1]) == "nil" and type(result[2]) == "number" then
        module.errno = result[2]
    end
    return table.unpack(result)
end

-- consolidated into single constructor
local _core_nameToOID = module.oidFromName
module.oidFromName = nil

-- makes code more readable
local sysctl    = module.sysctl
local types     = ls.makeConstantsTable(module.types)     -- so we can reverse lookup internally
local flags     = ls.makeConstantsTable(module.flags)     -- so we can reverse lookup internally
local typeSizes = ls.makeConstantsTable(module.typeSizes) -- so we can reverse lookup internally

-- Apple adds entries to the user node but doesn't register the details, so this
-- node has to be handled specially. The information is needed in this file, but
-- has no real benefit publicly.
local userFields = module.userFields
module.userFields = nil
local userFieldsName = userFields[0][1]
local userFieldsNode = userFields[0][2]

local deNullTerminateString = function(str)
    -- _sysctl returns strings with null terminator as part of the data package
    while str:sub(#str) == "\0" do str = str:sub(1, -2) end
    return str
end

local validateOID = function(oid)
    if type(oid) == "table" then
        local arrayCount = #oid
        local tableCount = 0
        local k, v = next(oid)
        while k ~= nil do
            tableCount = tableCount + 1
            k, v = next(oid, k)
        end
        if arrayCount == tableCount then
            local good = true
            for i,v in ipairs(oid) do
                if math.type(v) ~= "integer" then
                    good = false
                    break
                end
            end
            if good then return true end
        end
    end
    return false
end

-- predeclare so they can be referred to in oidMT methods
local _sysctl_debug, _sysctl_name, _sysctl_next, _sysctl_name2oid, _sysctl_oidfmt

local oidName = function(oid)
    local data, errno = nil, nil

    if oid[1] == userFieldsNode and #oid > 1 then
        -- Support for CTL_USER
        data, unk = userFieldsName, 2
        if oid[2] ~= 0 and oid[2] <= #userFields then
            data = data .. "." .. userFields[oid[2]][1]
            unk = 3
        end
        -- mimic sysctl.name's pattern of appending unknown oid entities as numbers
        for i = unk, #oid, 1 do data = data .. "." .. tostring(oid[i]) end
    else
        local command = _sysctl_name .. oid
        data, errno = command:sysctl()
    end

    if data then
        data = deNullTerminateString(data)
        return data
    else
        return nil, errno
    end
end

local oidType = function(oid)
    local command = _sysctl_oidfmt .. oid
    local data, errno = command:sysctl()
    local num, fmt = nil, nil

    if data and #data >= 4 then
        num, fmt = string.unpack("Ic" .. tostring(#data - 4), data)
        fmt = deNullTerminateString(fmt)
    elseif errno == ERROR.ENOENT then
        -- Support for CTL_USER
        if #oid == 2 and oid[1] == userFieldsNode and oid[2] ~= 0 and oid[2] <= #userFields then
            num, fmt, errno = userFields[oid[2]][2], "", nil
            module.errno = nil
        end
    end

    if num and fmt then
        -- Map Darwin sysctl types to FreeBSD types.
        -- - 0 with "I" -> CTLTYPE_INT
        -- - 0 with "S," -> CTLTYPE_STRUCT
        -- - CTLTYPE_INT with "IU" -> CTLTYPE_UINT
        -- - CTLTYPE_INT with "L" -> CTLTYPE_LONG
        -- - CTLTYPE_QUAD -> CTLTYPE_S64
        -- - CTLTYPE_QUAD with "*U" -> CTLTYPE_U64
        local baseType = num & types.typeMask
        if baseType == 0 or baseType == types.int then
            if fmt:match("^S") then
                num = num & ~types.typeMask | types.struct
            elseif fmt:match("^I") then
                num = num & ~types.typeMask | ((fmt:sub(2,2) == "U") and types.unsignedInt or types.int)
            elseif fmt:match("^L") then
                num = num & ~types.typeMask | ((fmt:sub(2,2) == "U") and types.unsignedLong or types.long)
            end
        elseif baseType == types.quad then
            num = num & ~types.typeMask | ((fmt:match("U")) and types.unsigned64 or types.signed64)
        end

        return num, fmt
    else
        return nil, errno
    end
end

local oidValue = function(oid, raw)
    local num, fmt = oid:type()
    if not num then return nil, fmt end
    local ctltype = num & types.typeMask

    local data, errno = oid:sysctl()
    if errno then
        if ctltype == types.node then
            module.errno = nil
            return "*NODE*"
        else
            return nil, errno
        end
    end

    if raw then return data end

    if not data then return nil end

    if ctltype == types.node then
        return "*NODE:" .. data
    elseif ctltype == types.string then
        return deNullTerminateString(data)
    elseif ctltype == types.int then          return conversions.genericInt(typeSizes.int, data)
    elseif ctltype == types.unsignedInt then  return conversions.genericInt(typeSizes.unsignedInt, data)
    elseif ctltype == types.long then         return conversions.genericInt(typeSizes.long, data)
    elseif ctltype == types.unsignedLong then return conversions.genericInt(typeSizes.unsignedLong, data)
    elseif ctltype == types.signed64 then     return conversions.genericInt(typeSizes.signed64, data)
    elseif ctltype == types.unsigned64 then   return conversions.genericInt(typeSizes.unsigned64, data)
    elseif ctltype == types.opaque then
        local helperFN = conversions[fmt:gsub(",", "_"):gsub(" ", "_")]
        if not helperFN then helperFN = conversions.quadMappings[fmt] end

        if helperFN then
            return helperFN(data)
        end
    -- else fall through
    end

    return data
end

local oidWalk = function(oid, captureFn) -- similar to sysctl_all in sysctl.c
    assert(
        type(captureFn) == "function" or type(captureFn) == "nil" or ((getmetatable(captureFn) or {}).__call),
        "captureFn must be nil or a function"
    )

    local results = {}

    if (#oid == 0 or (#oid == 1 and oid[1] == userFieldsNode)) then
        -- Support for CTL_USER
        for i, v in ipairs(userFields) do
            local nextOID = makeOID({ userFieldsNode, i })

            if captureFn then
                local status = captureFn(oid, nextOID)
                if type(status) ~= "boolean" and type(status) ~= "nil" then
                    table.insert(results, status)
                elseif status then
                    table.insert(results, nextOID)
                end
            else
                table.insert(results, nextOID)
            end
        end
    end

    local command = _sysctl_next .. (#oid > 0 and oid or { 1 })
    while true do
        local data, errno = command:sysctl()
        if errno then
            if errno == ERROR.ENOENT then -- exit because done
                module.errno = nil
                break
            end
            return nil, errno -- exit because error
        end

        local len = #data / 4
        if len < #oid then break end -- exit because current branch shorter than specified branch

        local fmtStg = string.rep("I", len)
        local nextOID = makeOID({ string.unpack(fmtStg, data) })
        table.remove(nextOID) -- remove position from string.unpack results

        local withinBranch = true
        for i = 1, #oid, 1 do
            if oid[i] ~= nextOID[i] then
                withinBranch = false
                break
            end
        end
        if not withinBranch then break end -- exit because no longer part of specified branch

        if captureFn then
            local status = captureFn(oid, nextOID)
            if type(status) ~= "boolean" and type(status) ~= "nil" then
                table.insert(results, status)
            elseif status then
                table.insert(results, nextOID)
            end
        else
            table.insert(results, nextOID)
        end
        command = _sysctl_next .. nextOID
    end

    if #results > 0 then
        return setmetatable(results, { __tostring = function(self)
            local rep = self[1] and tostring(self[1]) or ""
            for i = 2, #self, 1 do rep = rep .. ", " .. tostring(self[i]) end
            return "{ " .. (#rep > 0 and (rep .. " ") or "") .. "}"
        end })
    else
        return nil
    end
end

local oidChildren = function(oid, incMasked)
    incMasked = incMasked and true or false

    local entriesSeen, idxOfInterest = {}, #oid + 1
    return oid:walk(function(self, newOid)
        if not entriesSeen[newOid[idxOfInterest]] then
            entriesSeen[newOid[idxOfInterest]] = true

            -- sysctl.next doesn't return "nodes" but rather the first child of the node,
            -- so prune the part of the oid that represents the child
            local wantedOID = makeOID(newOid) -- make a copy
            while #wantedOID > idxOfInterest do table.remove(wantedOID) end

            if not incMasked then
                local num, fmt = wantedOID:type()
                if num and (num & flags.masked == flags.masked) then
                    return false
                end
            end

            return wantedOID
        else
            return false
        end
    end, oid)
end

local nodeMT
nodeMT = {
    __name = USERDATA_TAG .. ".node",
    __index = function(self, key)
        local children = self._oid:children()
        for i,v in ipairs(children or {}) do
            if v:name():match("%.?([%w_]+)$") == key then
                local num, fmt = v:type()
                if num and num & types.typeMask == types.node then
                    return setmetatable({ _oid = v }, nodeMT)
                else
                    return v:value()
                end
            end
        end
        return nil
    end,
    __newindex = function(self, key, value)
        error(string.format("%s interface is read-only", nodeMT.__name), 2)
    end,
    __pairs = function(self)
        local children = self._oid:children()
        local names = {}
        for i, v in ipairs(children) do
            names[v:name():match("%.?([%w_]+)$")] = i
        end

        return function(_, k)
            k, _ = next(names, k)
            local v = k and self[k] or nil
            return k, v
        end, self, nil
    end,
    __tostring = function(self)
        local rep = self._oid[1] and tostring(self._oid[1]) or ""
        for i = 2, #self._oid, 1 do rep = rep .. "." .. tostring(self._oid[i]) end
        return nodeMT.__name .. " [ " .. (#rep > 0 and (rep .. " ") or "") .. "] " .. string.format("%p", self)
    end,
    __eq = function(self, other)
        return getmetatable(self) == getmetatable(other) and self._oid == other._oid
    end,
}

local oidAsNode = function(oid)
    local num, fmt = oid:type()
    if #oid == 0 or (num and num & types.typeMask == types.node) then
        return setmetatable({ _oid = oid }, nodeMT)
    else
        return oid:value()
    end
end

-- metatable for OID tables
local oidMT
oidMT = {
    __name = USERDATA_TAG .. ".oid",
    __concat = function(a, b)
        local c = {}
        a = (type(a) == "table") and a or { a }
        b = (type(b) == "table") and b or { b }
        c = table.move(a, 1, #a, #c + 1, c)
        c = table.move(b, 1, #b, #c + 1, c)
        return setmetatable(c, oidMT)
    end,
    __index = {
        name     = oidName,
        type     = oidType,
        value    = oidValue,
        walk     = oidWalk,
        children = oidChildren,
        asNode   = oidAsNode,
        sysctl   = sysctl,
    },
    __tostring = function(self)
        local rep = self[1] and tostring(self[1]) or ""
        for i = 2, #self, 1 do rep = rep .. "." .. tostring(self[i]) end
        -- undecided on preference...
        return oidMT.__name .. " [ " .. (#rep > 0 and (rep .. " ") or "") .. "] " .. string.format("%p", self)
--         return "[ " .. (#rep > 0 and (rep .. " ") or "") .. "]"
    end,
    __eq = function(self, other)
        if getmetatable(self) == getmetatable(other) and #self == #other then
            for i,v in ipairs(self) do
                if v ~= other[i] then return false end
            end
            return true
        end
        return false
    end,
    __newindex = function(self, key, value)
        local errMsg = nil
        if math.type(key) ~= "integer" then
            errMsg = "index must be an integer"
        elseif math.type(value) ~= "integer" then
            errMsg = "value must be an integer"
        elseif key < 1 or key > #self + 1 then
            errMsg = string.format("index must be between 1 and %d inclusive", #self + 1)
        end

        if errMsg then
            error(string.format("%s: %s", oidMT.__name, errMsg), 2)
        else
            rawset(self, key, value)
        end
    end,
    __pairs = function(self) return pairs(oidMT.__index) end, -- helps with autocompletion
}

makeOID = function(...)
    local oid = { ... }
    if #oid == 1 and type(oid[1]) == "table" then
        oid = table.move(oid[1], 1, #oid[1], 1, {})
    end

    if #oid == 1 and type(oid[1]) == "string" then
        local name = oid[1]
        local oidString = name:match("^%[? *([%d%.]+) *%]?$")
        if oidString then
            oid = {}
            for d in oidString:gmatch("(%d+)%.?") do
                table.insert(oid, tonumber(d))
            end
        else
            module.errno = nil
            local ans, errno = _core_nameToOID(name)

            if errno == ERROR.ENOENT then
                -- Support for CTL_USER
                local userFieldsNameLen = #userFieldsName
                if name:match("^" .. userFieldsName) then
                    if name:sub(userFieldsNameLen + 1, userFieldsNameLen + 1) == "." then
                        local subPath = name:sub(userFieldsNameLen + 2)
                        for i, v in ipairs(userFields) do
                            if v[1] == subPath then
                                ans, errno = { userFieldsNode, i }, nil
                                break
                            end
                        end
                    elseif name == userFieldsName then
                        ans, errno = { userFieldsNode }, nil
                    end
                end
            end
            module.errno = errno

            if type(ans) == "table" then
                oid = ans
            else
                return nil, errno
            end
        end
    end

    assert(validateOID(oid), "expected array table of integers")

    return setmetatable(oid, oidMT)
end

-- define special OIDs used for querying data about sysctl entities
--     predeclared local above
_sysctl_debug    = makeOID{ 0, 0 } -- dumps info to kernel log about registered nodes; not really useful
_sysctl_name     = makeOID{ 0, 1 }
_sysctl_next     = makeOID{ 0, 2 }
_sysctl_name2oid = makeOID{ 0, 3 } -- sysctlnametomib is easier
_sysctl_oidfmt   = makeOID{ 0, 4 }

-- Public interface ------------------------------------------------------

module.errno = nil

module.types      = types
module.flags      = flags
module.typeSizes = typeSizes

-- lock this table a little differently
local systemOIDs = {
    -- remake the oids (which makes a copy internally) so that if they *do* change the sub-tables
    -- (making oidMT support a RO flag would be annoyingly tedious at the moment, maybe later...)
    -- they don't screw up the internal working of the module
    debug    = makeOID(_sysctl_debug),
    name     = makeOID(_sysctl_name),
    next     = makeOID(_sysctl_next),
    name2oid = makeOID(_sysctl_name2oid),
    oidfmt   = makeOID(_sysctl_oidfmt),
}
module.internals = setmetatable({}, {
    __index = function(_, key)
        return systemOIDs[key]
    end,
    __newindex = function(_, _, _)
        error("attempt to modify a table of constants",2)
    end,
    __pairs = function(_) return pairs(systemOIDs) end,
    __tostring = function(_)
        local maxWidth = 0
        for k,_ in pairs(systemOIDs) do maxWidth = math.max(maxWidth, #k) end
        local result = ""
        for k,v in fnutils.sortByKeys(systemOIDs) do
            result = result .. string.format("%-" .. tostring(maxWidth) .. "s %s\n", k, tostring(v))
        end
        return result
    end
})

module.oid = makeOID

module.node = function(...) return makeOID(...):asNode() end

module.decodeType = function(value, str)
    value = value or 0
    assert(math.type(value) == "integer", "expected integer")

    -- no reason to save it if it's empty
    if str == "" then str = nil end

    local oidType  = value & types.typeMask
    local oidFlags = value & ~types.typeMask

    local skipFlags = { "readWrite" }
    local result = {
        _value    = value,
        _type     = oidType,
        _flags    = oidFlags,
        _fmt      = str,
        _typename = types[oidType] or "unknown",
    }

    for k,v in pairs(flags) do
        if not fnutils.contains(skipFlags, k) then
            if (oidFlags & v) == v then
                result[k] = true
                oidFlags = oidFlags - v
            end
        end
    end

    if oidFlags ~= 0 then result._undecodedFlags = oidFlags end
    return result
end

module.decodeError = function(err)
    err = err or errno
    return ERROR_MSG[err] or string.format("unknown system error %d", err)
end

module.walk = function(...)
    local oid = { ... }

    local fn = (#oid > 0) and (type(oid[#oid]) == "function" or
                               type(oid[#oid]) == "nil" or
                               ((getmetatable(oid[#oid]) or {}).__call))
                          and table.remove(oid) or nil

    if #oid == 1 and (type(oid[1]) == "table" or type(oid[1]) == "string") then oid = oid[1] end

    return makeOID(oid):walk(fn)
end

module.sysctl_aN = function(...)
    local oid = { ... }

    local incMasked = (#oid > 0) and (type(oid[#oid]) == "boolean") and table.remove(oid) or false

    if #oid == 1 and (type(oid[1]) == "table" or type(oid[1]) == "string") then oid = oid[1] end

    return makeOID(oid):walk(function(self, newOid)
        local status = true
        if not incMask then
            local num, fmt = newOid:type()
            status = not (num and (num & flags.masked == flags.masked))
        end

        return status and newOid:name() or false
    end)
end

module.sysctl_ao = function(...)
    local interim = module.sysctl_aN(...)
    local results = {}
    for _,v in ipairs(interim) do
        local oid = makeOID(v)
        local num, fmt = oid:type()
        local value = oid:value()
        if num and num & types.typeMask == types.node and value == "*NODE*" then value = nil end
        results[v] = value
    end
    return setmetatable(results, { __tostring = inspect })
end

-- Return Module Object --------------------------------------------------

return setmetatable(module, { __call = function(_, ...) return makeOID(...) end })
