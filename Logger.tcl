package require logger
package require http

package provide ElectricCommander::Logger 0.0.1

namespace eval ::ElectricCommander::Logger {

    proc init { service { level {} } } {

        if { $level eq "" } {
            set level "error"
        } elseif { [string is true -strict $level] } {
            set level "debug"
        } elseif { [string is false -strict $level] } {
            set level "emergency"
        }

        set log [::logger::init $service]

        ${log}::setlevel $level

        namespace eval ::logger::tree::EC {
            proc stdoutcmd {level text} {
                variable service
                puts [format {[%s] [%-11s] [%s] %s} [clock format [clock seconds]] $service $level $text]
            }

            proc stderrcmd {level text} {
                variable service
                puts stderr [format {[%s] [%-11s] [%s] %s} [clock format [clock seconds]] $service $level $text]
            }
        }

        if { $level eq "debug" } {

            set ::httplog [::logger::init EC::HTTP]
            proc ::http::Log { args } {
                [set ::httplog]::debug $args
            }

        }

        return $log

    }

}