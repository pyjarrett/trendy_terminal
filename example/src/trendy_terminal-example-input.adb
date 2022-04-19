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
        subtype Key_Enter is Integer with
            Static_Predicate => Key_Enter in 10 | 13;
    begin
        loop
            declare
                Input     : constant ASU.Unbounded_String := ASU.To_Unbounded_String (TT.Platform.Get_Input);
            begin
                AIO.New_Line;
                if Trendy_Terminal.Maps.Is_Key (ASU.To_String (Input)) then
                    AIO.Put_Line (TT.Maps.Key'Image (Trendy_Terminal.Maps.Key_For (ASU.To_String (Input))));
                end if;

                for X in 1 .. ASU.Length (Input) loop
                    if Character'Pos (ASU.Element (Input, X)) in Key_Enter then
                        return;
                    end if;
                    AIO.Put_Line (Character'Pos (ASU.Element (Input, X))'Image);
                end loop;
            end;
        end loop;
    end Run_Print_Input;

end Trendy_Terminal.Example.Input;
