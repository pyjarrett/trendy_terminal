with Ada.Text_IO;
with Trendy_Terminal.VT100;

package body Trendy_Terminal.Example.Input is

    package AIO renames Ada.Text_IO;
    package TT renames Trendy_Terminal;

    procedure Run_Print_Input is
    begin
        loop
            declare
                Input     : constant String := TT.Get_Input;
                Key_Enter : constant := 13;
            begin
                AIO.New_Line;
                for X in 1 .. Input'Length loop
                    if Character'Pos (Input (X)) = Key_Enter then
                        return;
                    end if;
                    AIO.Put_Line (Character'Pos (Input (X))'Image);
                end loop;
            end;
        end loop;
    end Run_Print_Input;

    procedure Print_Cursor_Position is
        Pos : constant Trendy_Terminal.Cursor_Position :=
           Trendy_Terminal.Get_Cursor_Position;
    begin
        Ada.Text_IO.Put_Line (Pos.Row'Image & " " & Pos.Col'Image);
    end Print_Cursor_Position;

    procedure Run_Terminal_Editing is
        Input : constant String := Trendy_Terminal.Debug_Get_Line;
    begin
        Ada.Text_IO.New_Line(2);
        Ada.Text_IO.Put_Line ("Final input: " & Input);
    end Run_Terminal_Editing;

end Trendy_Terminal.Example.Input;
