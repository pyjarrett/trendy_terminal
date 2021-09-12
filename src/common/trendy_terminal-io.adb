with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Interfaces.C.Strings;

with Trendy_Terminal.Input;
with Trendy_Terminal.Maps;
with Trendy_Terminal.VT100;

package body Trendy_Terminal.IO is

    procedure Put (S : ASU.Unbounded_String) is
    begin
        Put (ASU.To_String(S));
    end Put;

    procedure Put_Line(S : String) is
    begin
        Put (S & Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.FF);
    end Put_Line;

    procedure Put_Line (S : ASU.Unbounded_String) is
    begin
        Put_Line (ASU.To_String(S));
    end Put_Line;

    procedure Clear_Line is
    begin
        VT100.Clear_Line;
    end Clear_Line;

    procedure New_Line (Num_Lines : Positive) is
    begin
        -- TODO: very inefficient
        for I in 1 .. Num_Lines loop
            Put_Line ("");
        end loop;
    end New_Line;

    procedure Set_Col (Column : Positive) is
        Cursor_Pos : Cursor_Position := Get_Cursor_Position;
    begin
        Cursor_Pos.Col := Column;
        VT100.Set_Cursor_Position (Cursor_Pos);
    end Set_Col;

    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    function Get_Line(Format_Fn     : Format_Function := null;
                      Completion_Fn : Completion_Function := null) return String
    is
        package TTI renames Trendy_Terminal.Input;
        use Trendy_Terminal.Maps;
        use all type Interfaces.C.int;
        use all type ASU.Unbounded_String;

        Input_Line : ASU.Unbounded_String;
        Input      : Interfaces.C.int;
        Key_CR     : constant := 10;
        Key_FF     : constant := 13;
        KM         : constant Trendy_Terminal.Maps.Key_Maps.Map := Maps.Make_Key_Map;
        MK         : constant Trendy_Terminal.Maps.Inverse_Key_Maps.Map := Maps.Make_Key_Lookup_Map;
        L          : Trendy_Terminal.Input.Line_Input;
        Line_Pos   : constant Cursor_Position := Get_Cursor_Position;
        Edit_Pos   : Cursor_Position := Line_Pos;

        -- Prints an updated input line at the given starting position.
        procedure Print_Line (Pos : Cursor_Position; S : String) is
        begin
            VT100.Set_Cursor_Position (Pos);
            VT100.Clear_Line;
            Put (S);
        end Print_Line;
    begin
        loop
            -- Clear anything which has been printed and then print the current
            -- state of the line.

            if Format_Fn /= null then
                Print_Line (Line_Pos, Format_Fn (TTI.Current (L)));
            else
                Print_Line (Line_Pos, TTI.Current (L));
            end if;

            Edit_Pos.Row := Line_Pos.Row;
            Edit_Pos.Col := TTI.Cursor_Index(L);
            VT100.Set_Cursor_Position (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Platform.Get_Input);

            if MK(Key_Left) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Left);
            elsif MK(Key_Right) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Right);
            elsif MK(Key_Backspace) = Input_Line then
                TTI.Backspace (L);
            elsif MK(Key_Delete) = Input_Line then
                TTI.Delete (L);
            elsif MK(Key_Tab) = Input_Line then
                -- Do tab completion on the line
                if Completion_Fn /= null then
                    -- Adjust the cursor position?
                    null;
                end if;
            elsif ASU.Length (Input_Line) = 1 then
                Input := Character'Pos(ASU.Element(Input_Line, 1));

                -- Line has been finished.
                if Input = Key_CR or else Input = Key_FF then
                    return TTI.Current(L);
                end if;
            end if;

            -- Actual text was inserted.
            -- TODO: Maybe add a "replace" mode?
            if not KM.Contains (Input_Line) then
                TTI.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Get_Line;

    function Get_Cursor_Position return Cursor_Position is
    begin
        loop
            Platform.Clear_Input_Buffer;
            VT100.Report_Cursor_Position;
            declare
                Result : constant String := Platform.Get_Input;
                Semicolon_Index : constant Natural := Ada.Strings.Fixed.Index(Result, ";", 1);
                Row : Integer := 1;
                Col : Integer := 1;
            begin
                -- The cursor position is reported as
                -- ESC [ ROW ; COL R

                -- May throw on bad parse.
                Row := Integer'Value(Result(3 .. Semicolon_Index - 1));
                Col := Integer'Value(Result(Semicolon_Index + 1 .. Result'Length - 1));

                return Cursor_Position'(Row => Row, Col => Col);
            exception
                -- Bad parse due to existing input on the line.
                when Constraint_Error =>
                    null;
            end;
        end loop;
    end Get_Cursor_Position;

end Trendy_Terminal.IO;
