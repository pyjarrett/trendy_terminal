with Trendy_Terminal;

with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Progress_Indicators.Bars;
with Progress_Indicators.Spinners;

procedure Trendy_Terminal_Test is
    use Progress_Indicators.Spinners;
    use Progress_Indicators.Bars;

    S : Spinner := Make;

    Bar_Widths : constant array (Natural range <>) of Natural := (100, 50, 10, 5, 0);

    Show_Progress_Test : constant Boolean := True;
begin
    if not Trendy_Terminal.Init then
        Ada.Text_IO.Put_Line ("Trendy terminal failed to initialized.");
    end if;

    if True then
        Trendy_Terminal.Set (Trendy_Terminal.Echo, False);
        Trendy_Terminal.Set (Trendy_Terminal.Line_Input, False);
        Trendy_Terminal.Set (Trendy_Terminal.Escape_Sequences, True);

        if Show_Progress_Test then
            for Width of Bar_Widths loop
                for I in Percentage'(0) .. 100 loop
                    delay 0.005;
                    Ada.Text_IO.Put (Get_Bar (I, Width));
                end loop;
                Ada.Text_IO.New_Line;
            end loop;

            for I in 1 .. 100 loop
                delay 0.005;
                Tick (S);
                Ada.Text_IO.Put (Value (S));
            end loop;
        end if;

        declare
            Last : Character;
        begin
            loop
                Ada.Text_IO.Get_Immediate (Last);
                Ada.Text_IO.Put_Line ("CHAR : " & Last'Image);
                exit when Last = Ada.Characters.Latin_1.CR;
                exit when Last = Ada.Characters.Latin_1.LF;
            end loop;
        end;
    end if;

    Trendy_Terminal.Shutdown;
end Trendy_Terminal_Test;
