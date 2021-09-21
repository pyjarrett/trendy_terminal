with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Trendy_Terminal.Maps;
with Trendy_Terminal.Platform;
with Trendy_Terminal.VT100;

package body Trendy_Terminal.Example.Input is

    package AIO renames Ada.Text_IO;
    package ASU renames Ada.Strings.Unbounded;
    package TT renames Trendy_Terminal;

    -- Name that input!
    procedure Run_Print_Input is
    begin
        loop
            declare
                Input     : constant ASU.Unbounded_String := ASU.To_Unbounded_String (TT.Platform.Get_Input);
                Key_Enter : constant := 13;
                Inverse   : constant Trendy_Terminal.Maps.Key_Maps.Map := Trendy_Terminal.Maps.Make_Key_Map;
            begin
                AIO.New_Line;
                if Inverse.Contains (Input) then
                    AIO.Put_Line (TT.Maps.Key'Image(Inverse(Input)));
                end if;
                for X in 1 .. ASU.Length (Input) loop
                    if Character'Pos (ASU.Element (Input, X)) = Key_Enter then
                        return;
                    end if;
                    AIO.Put_Line (Character'Pos (ASU.Element (Input, X))'Image);
                end loop;
            end;
        end loop;
    end Run_Print_Input;

end Trendy_Terminal.Example.Input;
