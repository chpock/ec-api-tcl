
package provide ElectricCommander::Arguments 0.0.1

namespace eval ::ElectricCommander::Arguments {

    variable Arguments

    variable DefaultFlags {
        wrap 0 locator 0
    }

    proc get_required { command } {
        variable Arguments
        return [lindex $Arguments($command) 0]
    }

    proc get_optional { command } {
        variable Arguments
        return [lindex $Arguments($command) 1]
    }

    proc get_flags { command } {
        variable Arguments
        variable DefaultFlags
        return [dict merge $DefaultFlags [lindex $Arguments($command) 2]]
    }

    proc all_commands { } {
        variable Arguments
        return [array names Arguments]
    }

}


set fid [open [file join [file dirname [info script]] ArgumentsList.tcl] r]
set data [list]
while { [gets $fid line] != -1 } {
    set line [string trimleft $line]
    if { $line ne "" && [string index $line 0] ne "#" } {
        lappend data $line
    }
}
close $fid
array set ::ElectricCommander::Arguments::Arguments [join $data \n]

unset data line fid

return

#==========================================================
# ArgumentsList.tcl generated by following ec-perl script
#==========================================================

#!/usr/bin/env ec-perl

use ElectricCommander::Arguments;

print "# Generated by ectool v" . $ElectricCommander::Arguments::VERSION . "\n";
print "# " . localtime() . "\n\n";

foreach my $command (keys %ElectricCommander::Arguments) {
    print $command . " { \n";

    print "    {" . join(' ', @{$ElectricCommander::Arguments{$command}[0]}) . "}\n";
    print "    {" . join(' ', @{$ElectricCommander::Arguments{$command}[1]}) . "}\n";

    my $flags = $ElectricCommander::Arguments{$command}[2];

    my @out = [];
    print "    {";
    my $first = 1;
    foreach my $flag (keys %{$flags}) {
        if (!$first) {
            print " ";
        } else {
            $first = 0;
        }
        print $flag;
        if (ref($flags->{$flag}) eq "ARRAY") {
            print " {" . join(' ', @{$flags->{$flag}}) . "}";
        } else {
            print " " . $flags->{$flag};
        }
    }
    print "}\n";

    print "}\n";
}
