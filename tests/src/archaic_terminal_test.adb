with Ada.Text_IO;

with Archaic_Terminal;

with Progress_Indicators.Bars;
with Progress_Indicators.Spinners;

procedure Archaic_Terminal_Test is
    use Progress_Indicators.Spinners;
    use Progress_Indicators.Bars;

    S : Spinner := Make;

    Bar_Widths : constant array (Natural range <>) of Natural := (100, 50, 10, 5, 0);
begin
    if not Archaic_Terminal.Init then
        Ada.Text_IO.Put_Line ("Archaic terminal failed to initialized.");
    end if;

    Ada.Text_IO.Put_Line ("HERE");
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
    Archaic_Terminal.Shutdown;
end Archaic_Terminal_Test;
