with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System;

with Ada.Strings.Fixed;
with Interfaces.C.Strings;

with Trendy_Terminal.Input;
with Trendy_Terminal.Linux;
with Trendy_Terminal.Maps;
with Trendy_Terminal.VT100;

package body Trendy_Terminal.Platform is
    package AIO renames Ada.Text_IO;

    use type Linux.BOOL;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    type IOStream is record
        File            : Linux.FILE_Ptr;
        File_Descriptor : Linux.FD;
        Settings        : aliased Linux.Termios;
    end record;

    ---------------------------------------------------------------------------
    -- Original settings
    ---------------------------------------------------------------------------
    Original_Input_Setting, Original_Output_Setting, Original_Error_Setting :
    Linux.Termios;

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input, Std_Output, Std_Error : IOStream;

    function Load_Terminal (File_Descriptor : Linux.FD; Terminal : not null access
        Linux.Termios) return Boolean is
    begin
        if Linux.isatty (File_Descriptor) = 0 then
            return False;
        end if;
        if Linux.tcgetattr (File_Descriptor, Terminal.all'Address) /= 0 then
            return False;
        end if;
        return True;
    end Load_Terminal;

    function Make_Handle (File_Descriptor : Linux.FILE_Ptr; Handle : out IOStream) return Boolean is
    begin
        Handle.File := File_Descriptor;
        Handle.File_Descriptor := Linux.fileno (Handle.File);
        return Load_Terminal (Handle.File_Descriptor, Handle.Settings'Unchecked_Access);
    end Make_Handle;

    procedure Require_Settings_Change (Stream : IOStream) with
        Pre => Linux.isatty (Stream.File_Descriptor) /= 0
    is
    begin
        if Linux.tcsetattr (Stream.File_Descriptor, Linux.TCSANOW, Stream.Settings'Address) /= 0 then
            AIO.Put_Line ("Unable to change settings.");
        end if;
    end Require_Settings_Change;

    function Init return Boolean is
        -- Initializes and captures the original settings for the terminal so they can
        -- be restored when the system is shutdown.
    begin
        if not Make_Handle (Linux.stdin, Std_Input) or else not Make_Handle (Linux.stdout, Std_Output)
            or else not Make_Handle (Linux.stderr, Std_Error) then
            AIO.Put_Line ("Unable to get standard stream handles.");
        end if;

        -- Save the startup settings.
        Original_Input_Setting  := Std_Input.Settings;
        Original_Output_Setting := Std_Output.Settings;
        Original_Error_Setting  := Std_Error.Settings;
        return True;
    end Init;

    procedure Shutdown is
    begin
        Std_Input.Settings  := Original_Input_Setting;
        Std_Output.Settings := Original_Output_Setting;
        Std_Error.Settings  := Original_Error_Setting;

        Require_Settings_Change (Std_Input);
        Require_Settings_Change (Std_Output);
        Require_Settings_Change (Std_Error);
    end Shutdown;

    procedure Set (Setting : Platform.Input_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Platform.Echo =>
                Std_Input.Settings.c_lflag (Linux.ECHO) := Enabled;
            when Platform.Line_Input =>
                Std_Input.Settings.c_lflag (Linux.ICANON) := Enabled;
        end case;
        Require_Settings_Change (Std_Input);
    end Set;

    procedure Set (Setting : Output_Setting; Enabled : Boolean) is
    begin
        pragma Unreferenced (Enabled);
        case Setting is
            when Escape_Sequences =>
                null;
                -- nothing to do here
        end case;
    end Set;

    procedure Put (C : Character) renames Ada.Text_IO.Put;
    procedure Put (S : String) renames Ada.Text_IO.Put;

    type VOIDP is new Interfaces.C.Strings.chars_ptr;
    function Read (File_Descriptor : Linux.FD; Buffer : VOIDP; Buffer_Size : Natural) return Integer
        with Import     => True,
             Convention => C;

    procedure Clear_Input_Buffer is
        Buffer_Size  : constant := 1024;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Chars_Read   : Integer;
        use all type Interfaces.C.size_t;
    begin
        -- Put something into the buffer to ensure it won't block.
        -- It'd be better to peek than do this, but that might fail on named
        -- pipes for inputs and this is just a simple, but hacky way of doing it.
        VT100.Report_Cursor_Position;
        loop
            Chars_Read := Read (Linux.fileno (Std_Input.File),
                VOIDP (Interfaces.C.Strings.To_Chars_Ptr (Buffer'Unchecked_Access)),
                Buffer_Size);
            exit when Chars_Read < Buffer_Size;
        end loop;
    end Clear_Input_Buffer;

    -- Gets an entire input line from one keypress.  E.g. all the characters
    -- received for a controlling keypress, such as an arrow key.
    function Get_Input return String is
        Buffer_Size  : constant := 512;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Chars_Read   : Integer;
        use all type Interfaces.C.size_t;
    begin
        Chars_Read := Read (Linux.fileno (Std_Input.File),
            VOIDP (Interfaces.C.Strings.To_Chars_Ptr (Buffer'Unchecked_Access)),
            Buffer_Size);
        if Chars_Read > 0 then
            return Interfaces.C.To_Ada(Buffer(1 .. Interfaces.C.size_t(Chars_Read) + 1));
        else
            return "";
        end if;
    end Get_Input;

    function End_Of_Line return String is
    begin
        return (1 => Ada.Characters.Latin_1.LF);
    end End_Of_Line;

    procedure Print_Configuration is
        function To_Integer is new Ada.Unchecked_Conversion (Linux.Local_Flags, Integer);
    begin
        Ada.Text_IO.Put_Line ("Input Mode:  " & To_Integer (Std_Input.Settings.c_lflag)'Image);
        Ada.Text_IO.Put_Line ("Output Mode: " & To_Integer (Std_Output.Settings.c_lflag)'Image);
        Ada.Text_IO.Put_Line ("Error Mode:  " & To_Integer (Std_Error.Settings.c_lflag)'Image);
    end Print_Configuration;

end Trendy_Terminal.Platform;
