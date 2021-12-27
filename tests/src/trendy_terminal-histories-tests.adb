with Trendy_Terminal.Histories;
with Trendy_Terminal.Lines.Line_Vectors;

with Trendy_Test.Assertions;
with Trendy_Test.Assertions.Integer_Assertions;

use Trendy_Test.Assertions;
use Trendy_Test.Assertions.Integer_Assertions;

package body Trendy_Terminal.Histories.Tests is

    procedure Assert_Contains
        (Op        : in out Trendy_Test.Operation'Class;
         Container : Lines.Line_Vectors.Vector;
         Item      : String) is
    begin
        Assert (Op, Container.Contains (Lines.Make (Item)));
    end Assert_Contains;

    procedure Assert_Not_Contains
        (Op        : in out Trendy_Test.Operation'Class;
         Container : Lines.Line_Vectors.Vector;
         Item      : String) is
    begin
        Assert (Op, not Container.Contains (Lines.Make (Item)));
    end Assert_Not_Contains;

    procedure Test_Simple_Completion (Op : in out Trendy_Test.Operation'Class) is
    begin
        Op.Register;

        declare
            Sample      : Histories.History;
            Completions : Lines.Line_Vectors.Vector;
        begin
            Assert_EQ (Op, Num_Entries (Sample), 0);

            Add (Sample, "this is a sample");
            Add (Sample, "this is a test");
            Assert_EQ (Op, Num_Entries (Sample), 2);

            Completions := Completions_Matching (Sample, "this is a ");
            Assert_EQ (Op, Integer (Lines.Line_Vectors.Length (Completions)), 2);
            Assert_Contains (Op, Completions, "this is a sample");
            Assert_Contains (Op, Completions, "this is a test");
            Assert_Not_Contains (Op, Completions, "this is a failure");
        end;
    end Test_Simple_Completion;

    ---------------------------------------------------------------------------
    -- Test Registry
    ---------------------------------------------------------------------------
    function All_Tests return Trendy_Test.Test_Group is (
        1 => Test_Simple_Completion'Access
        );

end Trendy_Terminal.Histories.Tests;
