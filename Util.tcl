package provide ElectricCommander::Util 0.0.1

namespace eval ::ElectricCommander::Util {

    proc maskPassword { data } {
        regsub -all {<password>.+?</password>} $data {<password>[PROTECTED]</password>} data
        regsub -all {"password"\s*:\s*"[^""]*"} $data {"password":"[PROTECTED]"} data
        return $data
    }

    proc maskSessionId { data } {
        regsub -all {sessionId=".+?"} $data {sessionId="[PROTECTED]"} data
        regsub -all {<sessionId>.+?</sessionId>} $data {<sessionId>[PROTECTED]</sessionId>} data
        return $data
    }

    proc maskSensitiveData { data } {
        return [maskPassword [maskSessionId $data]]
    }

    proc truncateString { str { len 4096 } } {
        if { [string length $str] > $len } {
            set str [string range $str 0 [expr { $len - 1 }]]
            append str "\n... Truncated at $len characters ..."
        }
        return $str
    }

    proc truncateAndMaskString { str { len 4096 } } {
        return [truncateString [maskSensitiveData $str] $len]
    }

}