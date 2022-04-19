with Ada.Text_IO;

with Trendy_Terminal.Environments;
with Trendy_Terminal.IO;
with Trendy_Terminal.Platform;
with Trendy_Terminal.Example.Input; use Trendy_Terminal.Example.Input;

procedure Trendy_Terminal_Example is
    Env : Trendy_Terminal.Environments.Environment;
begin
    if not Env.Is_Available then
        Ada.Text_IO.Put_Line ("Unable to initialize Trendy Terminal.");
        return;
    end if;

    Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Echo, False);
    Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Line_Input, False);
    Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Escape_Sequences, True);
    Trendy_Terminal.Platform.Set (Trendy_Terminal.Platform.Signals_As_Input, True);

    Trendy_Terminal.Platform.Print_Configuration;

    Trendy_Terminal.IO.Put_Line ("Hello, world.");
    Trendy_Terminal.IO.Put_Line ("Columns");
    Trendy_Terminal.IO.Put_Line ("12345678901234567890123456789012345678901234567890");
    Trendy_Terminal.IO.Put ("Move to column: ");
    Trendy_Terminal.IO.Set_Col (20);
    Trendy_Terminal.IO.Put_Line ("At column 20");

    Trendy_Terminal.Example.Input.Run_Print_Input;

end Trendy_Terminal_Example;
