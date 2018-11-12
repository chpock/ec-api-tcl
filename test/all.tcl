#!/usr/bin/env tclsh

package require tcltest
namespace import ::tcltest::*
configure -verbose {body pass skip start error} -tmpdir "./out"

set ::env(EC_TEST_CONFIG) {
    -server 192.168.5.139
    -userName admin
    -password changeme
    -testProject TclTestProject
}

runAllTests
