$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$sut = (Split-Path -Leaf $MyInvocation.MyCommand.Path).Replace(".Tests.", ".")
. "$here\$sut"

Describe "Add Numbers" {
    Context "When the input is an Empty string" {

        $return = AddNumbers("")

        It "should return 0" {
            $return | Should Be 0
        }
    }

    Context "When the input is a number" {

        $return = AddNumbers("5")

        It "should return the same number" {
            $return | Should Be 5
        }
    }

    Context "When the input are two numbers" {

        $return = AddNumbers("5,2")

        It "should execute TheFunction" {
            Assert-VerifiableMocks
        }

        It "should return the addition of the two numbers" {
            $return | Should Be 7
        }
    }
}