local sysctl  = require("hs.host.sysctl")
local fnutils = require("hs.fnutils")
local utf8    = require("hs.utf8")
local console = require("hs.console")

-- local hd = utf8.hexDump

local isZero = function(data)
    local allZeros = true
    for i = 1, #data, 1 do
        allZeros = string.byte(data, i) == 0
        if not allZeros then break end
    end
    return allZeros
end

local detabOutput = function(output)
    local lines = fnutils.split(output, "[\r\n]")

    local widths, outputTable = {}, {}

    for i = 1, #lines, 1 do
        outputTable[i] = fnutils.split(lines[i], "\t")
        for n, v in ipairs(outputTable[i]) do
            widths[n] = math.max((widths[n] or 0), #(v or ""))
            if widths[n] > 99 then
                widths[n] = 99
                print(string.format("WARNING: width for '%s' too long", v))
            end
        end
    end

    local result = ""
    for i = 1, #lines, 1 do
        local fmtStr = ""
        for n, v in ipairs(outputTable[i]) do
            fmtStr = fmtStr .. "%-" .. tostring(widths[n]) .. "s  "
        end
        fmtStr = fmtStr:sub(1, -3) .. "\n"
        result = result .. string.format(fmtStr, table.unpack(outputTable[i]))
    end
    return result
end

report = function(r)
    r = r or sysctl.walk()
    local results = "name\ttype\tfmt\tsLen\tempty\ttError\tvError\n\n"

    for i,v in ipairs(r) do
        local oid, oidErr = sysctl.oid(v)
        local nam = oid and oid:name() or v
        if oid then
            local typ, fmt    = oid:type()
            local typErr      = not typ and fmt or nil
            typErr = typErr and sysctl.decodeError(typErr)
            if typErr then fmt = nil end

            local val, valErr
            if typErr then
                -- :value uses :type internally, so try getting it directly
                val, valErr = sysctl.sysctl(oid)
                valErr = valErr and sysctl.decodeError(valErr) or "sysctl-direct only"
            else
                val, valErr = oid:value()
                valErr = valErr and sysctl.decodeError(valErr)
            end

            local isStringType = type(val) == "string"
            local len, zeroLen, zeroStruct
            if isStringType then
                len        = (val and #val)
                zeroLen    = (val and #val == 0)
                zeroStruct = (val and isZero(val))
            end

            local claimedType = (typ and typ & sysctl.types.typeMask) or 0
            if sysctl.types[claimedType] then
                claimedType = sysctl.types[claimedType]
            else
                claimedType = string.format("** unknown type %d", claimedType)
            end

            results = results .. string.format("%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
                nam,
                claimedType,
                (fmt and fmt or "n/a"),
                (len and tostring(len) or "n/a"),
                ((zeroLen and "empty") or (zeroStruct and "== { 0 }") or ""),
                (typErr and typErr or ""),
                (valErr and valErr or "")
            )
        else
            results = results .. string.format("%s\tnot found: %s\n", nam, sysctl.decodeError(oidErr))
        end
    end

    console.clearConsole()
    print(detabOutput(results))
end
