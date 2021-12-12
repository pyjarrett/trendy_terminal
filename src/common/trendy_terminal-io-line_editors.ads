with Trendy_Terminal.Histories;
with Trendy_Terminal.IO;
with Trendy_Terminal.Lines;

-- Things which can affect editing of lines within Trendy Terminal.
--
-- Line editing involves formatting, completion and handling of command history.
--
package Trendy_Terminal.IO.Line_Editors is

    -- Updates a line to be a formatted line.
    type Format_Function is access function (L : Lines.Line) return Lines.Line;

    -- Attempts to complete a line.
    --
    -- Completion_Index is the N'th attempted completion of the line.
    -- Shift-tab should decrease the Completion_Index,
    -- tab should increase the Completion_Index.
    type Completion_Function is access function (L : Lines.Line)
        return Lines.Line_Vectors.Vector;

    -- Line editing
    --
    -- A description of the elements involved to modify a line of text.
    type Line_Editor is interface;
    function Get_Line (Editor : in out Line_Editor'Class) return String;

    function Format   (E : in out Line_Editor; L : Lines.Line) return Lines.Line is abstract;
    function Complete (E : in out Line_Editor; L : Lines.Line) return Lines.Line_Vectors.Vector is abstract;
    procedure Submit  (E : in out Line_Editor; L : Lines.Line) is abstract;
    function Line_History (E : in out Line_Editor) return Trendy_Terminal.Histories.History_Access is abstract;

    type Stateless_Line_Editor is new Line_Editor with record
        Format_Fn     : Format_Function;
        Completion_Fn : Completion_Function;
        Line_History  : Histories.History_Access;
    end record;

    overriding
    function Format (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line;

    overriding
    function Complete (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line_Vectors.Vector;

    overriding
    procedure Submit (E: in out Stateless_Line_Editor; L : Lines.Line);

    overriding
    function Line_History (E : in out Stateless_Line_Editor) return Trendy_Terminal.Histories.History_Access is
        (E.Line_History);

    -- Helper to implicitly use a Stateless_Line_Editor
    function Get_Line (Format_Fn     : Format_Function := null;
                       Completion_Fn : Completion_Function := null;
                       Line_History  : Histories.History_Access := null) return String;

end Trendy_Terminal.IO.Line_Editors;
