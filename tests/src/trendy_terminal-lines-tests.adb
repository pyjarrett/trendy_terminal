with Trendy_Test.Assertions;

package body Trendy_Terminal.Lines.Tests is

    use Trendy_Test.Assertions;

    procedure Test_Make (Op : in out Trendy_Test.Operation'Class) is

    begin
        Op.Register;

        declare
            L : Line;
        begin
            L := Make("a simple line");
            Assert_EQ (Op, Current (L), "a simple line");
        end;
    end Test_Make;

    ---------------------------------------------------------------------------
    -- Test Registry
    ---------------------------------------------------------------------------
    function All_Tests return Trendy_Test.Test_Group is (
        1 => Test_Make'Access
        );

end Trendy_Terminal.Lines.Tests;
