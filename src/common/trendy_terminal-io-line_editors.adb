with Trendy_Terminal.Completions;
with Trendy_Terminal.Maps;
with Trendy_Terminal.Lines; use Trendy_Terminal.Lines;
with Trendy_Terminal.Lines.Line_Vectors;

package body Trendy_Terminal.IO.Line_Editors is

    function Should_Terminate_Input (Input_Line : ASU.Unbounded_String) return Boolean is
        Key_CR : constant := 10;
        Key_FF : constant := 13;
        Input  : constant Integer := Character'Pos(ASU.Element(Input_Line, 1));
    begin
        return Input = Key_CR or else Input = Key_FF;
    end Should_Terminate_Input;

    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    --
    -- Helper to implicitly use a Stateless_Line_Editor
    function Get_Line(Format_Fn     : Format_Function := null;
                      Completion_Fn : Completion_Function := null;
                      Line_History  : Histories.History_Access := null) return String
    is
        Editor : Stateless_Line_Editor := (Format_Fn     => Format_Fn,
                                           Completion_Fn => Completion_Fn,
                                           Line_History  =>    Line_History);
    begin
        return Get_Line (Editor);
    end Get_Line;

    function Get_Line (Editor : in out Line_Editor'Class) return String is
        use Trendy_Terminal.Maps;
        use all type ASU.Unbounded_String;
        use all type Ada.Containers.Count_Type;
        use Trendy_Terminal.Histories;

        Input_Line  : ASU.Unbounded_String;
        L           : Lines.Line;
        Line_Pos    : constant VT100.Cursor_Position := VT100.Get_Cursor_Position;
        Edit_Pos    : VT100.Cursor_Position := Line_Pos;
        Tab_Pos     : Integer := 1;

        Tab_Completions     : Trendy_Terminal.Completions.Completion_Set;
        History_Completions : Trendy_Terminal.Completions.Completion_Set;

        Reset_Keys : constant array(Positive range <>) of Key :=
            (Key_Left,
             Key_Right,
             Key_Backspace,
             Key_Delete,
             Key_Home,
             Key_End
            );
    begin
        Edit_Pos.Row := Line_Pos.Row;
        Trendy_Terminal.Completions.Clear (Tab_Completions);
        Trendy_Terminal.Completions.Clear (History_Completions);

        loop
            Rewrite_Line (Line_Pos, Lines.Current (Editor.Format (L)));

            Edit_Pos.Col := Lines.Get_Cursor_Index(L) + Line_Pos.Col - 1;
            VT100.Set_Cursor_Position (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Platform.Get_Input);

            if Maps.Sequence_For(Key_Left) = Input_Line then
                Lines.Move_Cursor(L, Lines.Left);
            elsif Maps.Sequence_For (Key_Right) = Input_Line then
                Lines.Move_Cursor(L, Lines.Right);
            elsif Maps.Sequence_For (Key_Backspace) = Input_Line then
                Lines.Backspace (L);
            elsif Maps.Sequence_For (Key_Delete) = Input_Line then
                Lines.Delete (L);
            elsif Maps.Sequence_For (Key_Home) = Input_Line then
                Lines.Set_Cursor_Index (L, 1);
            elsif Maps.Sequence_For (Key_End) = Input_Line then
                Lines.Set_Cursor_Index (L, Lines.Length (L) + 1);
            elsif Maps.Sequence_For (Key_Up) = Input_Line then
                -- Roll to the previous element in history
                Trendy_Terminal.Completions.Clear (Tab_Completions);
                if not Trendy_Terminal.Completions.Is_Empty (History_Completions) then
                    Trendy_Terminal.Completions.Move_Forward (History_Completions);
                elsif Editor.Line_History /= null then
                    Trendy_Terminal.Completions.Fill (
                        History_Completions, Trendy_Terminal.Histories.Completions_Matching (
                            Editor.Line_History.all, Lines.Current (L)));
                end if;

                if not Trendy_Terminal.Completions.Is_Empty (History_Completions) then
                    L := Lines.Make (Trendy_Terminal.Completions.Get_Current (History_Completions));
                end if;
            elsif Maps.Sequence_For (Key_Down) = Input_Line then
                Trendy_Terminal.Completions.Clear (Tab_Completions);
                if not Trendy_Terminal.Completions.Is_Empty (History_Completions) then
                    Trendy_Terminal.Completions.Move_Backward (History_Completions);
                else
                    if Editor.Line_History /= null then
                        Trendy_Terminal.Completions.Fill (
                            History_Completions, Trendy_Terminal.Histories.Completions_Matching (
                                Editor.Line_History.all, Lines.Current (L)));
                    end if;
                end if;

                if not Trendy_Terminal.Completions.Is_Empty (History_Completions) then
                    L := Lines.Make (Trendy_Terminal.Completions.Get_Current (History_Completions));
                end if;
            elsif Maps.Sequence_For (Key_Shift_Tab) = Input_Line then
                Trendy_Terminal.Completions.Clear (History_Completions);
                if not Trendy_Terminal.Completions.Is_Empty (Tab_Completions) then
                    Trendy_Terminal.Completions.Move_Backward (Tab_Completions);
                else
                    Trendy_Terminal.Completions.Fill (Tab_Completions, Editor.Complete (L));
                end if;

                if not Trendy_Terminal.Completions.Is_Empty (Tab_Completions) then
                    L := Lines.Make (Trendy_Terminal.Completions.Get_Current (Tab_Completions));
                end if;
            elsif Maps.Sequence_For (Key_Tab) = Input_Line then
                Trendy_Terminal.Completions.Clear (History_Completions);
                if not Trendy_Terminal.Completions.Is_Empty (Tab_Completions) then
                    Trendy_Terminal.Completions.Move_Forward (Tab_Completions);
                else
                    Trendy_Terminal.Completions.Fill (Tab_Completions, Editor.Complete (L));
                end if;

                if not Trendy_Terminal.Completions.Is_Empty (Tab_Completions) then
                    L := Lines.Make (Trendy_Terminal.Completions.Get_Current (Tab_Completions));
                end if;
            elsif Maps.Sequence_For (Key_Ctrl_C) = Input_Line then
                return "";
            elsif ASU.Length (Input_Line) = 1 and then Should_Terminate_Input (Input_Line) then
                -- TODO: this should only add the commadn if it was successful.
                Submit (Editor, L);
                return Lines.Current (L);
            elsif not Maps.Is_Key (ASU.To_String (Input_Line)) then
                -- Actual text was inserted, so restart completions.
                -- TODO: Maybe add a "replace" mode?
                Trendy_Terminal.Completions.Clear (Tab_Completions);
                Trendy_Terminal.Completions.Clear (History_Completions);
                Lines.Insert (L, ASU.To_String (Input_Line));
            end if;

            if Maps.Is_Key (ASU.To_String (Input_Line)) and then
                    (for some Key of Reset_Keys => Maps.Key_For (ASU.To_String (Input_Line)) = Key) then
                Trendy_Terminal.Completions.Clear (Tab_Completions);
                Trendy_Terminal.Completions.Clear (History_Completions);
            end if;
        end loop;
    end Get_Line;

    overriding
    function Format (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line is
        (if E.Format_Fn /= null then E.Format_Fn (L) else L);

    overriding
    function Complete (E : in out Stateless_Line_Editor; L : Lines.Line) return Lines.Line_Vectors.Vector is
        (if E.Completion_Fn /= null then E.Completion_Fn (L) else Lines.Line_Vectors.Empty_Vector);

    overriding
    procedure Submit (E: in out Stateless_Line_Editor; L : Lines.Line) is
    begin
        null;
    end Submit;

end Trendy_Terminal.IO.Line_Editors;
