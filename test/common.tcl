
package require tcltest

lappend auto_path ..
package require ElectricCommander

if { [info exists ::env(EC_TEST_CONFIG)] } {
    foreach { k v } $::env(EC_TEST_CONFIG) {
        set [string range $k 1 end] $v
    }
} else {
    error "Error: environment var 'env(EC_TEST_CONFIG)' is not defined"
}

proc test { name description args } {

    set test_path [file join \
        [::tcltest::temporaryDirectory] \
        $name \
    ]

    lappend ::__test_path $test_path

    set res [::tcltest::test $name description {*}$args]

    set ::__test_path [lrange $::__test_path 0 end-1]

    return $res

}

proc getTestFilename { name } {

    set test_path [lindex $::__test_path end]

    if { [file exists $test_path] } {
        file delete -force -- $test_path
    }

    file mkdir $test_path

    return [file join $test_path $name]

}

test "init - create EC object" {} -body {
    ElectricCommander new -default -server $::server -timeout 100 -retry 0 -debug 0
} -match glob -result "::oo::Obj*"

test "init - login" {} -body {
    login $::userName $::password
} -match regexp -result "^sessionId \[0-9A-Z\]{16} userName $::userName\$"

