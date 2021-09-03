with Ada.Text_IO;
with Trendy_Terminal;

with Trendy_Terminal.Example.Input; use Trendy_Terminal.Example.Input;

procedure Trendy_Terminal_Example is
begin
    if not Trendy_Terminal.Init then
        Ada.Text_IO.Put_Line ("Unable to initialize Trendy Terminal.");
        return;
    end if;

    Trendy_Terminal.Set (Trendy_Terminal.Echo, False);
    Trendy_Terminal.Set (Trendy_Terminal.Line_Input, False);
    Trendy_Terminal.Set (Trendy_Terminal.Escape_Sequences, True);

    -- Trendy_Terminal.Example.Input.Run_Print_Input;
    -- Trendy_Terminal.Example.Input.Print_Cursor_Position;
    Trendy_Terminal.Example.Input.Run_Terminal_Editing;
end Trendy_Terminal_Example;
