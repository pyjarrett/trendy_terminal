with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C;

package body Archaic_Terminal is

    package AIO renames Ada.Text_IO;

    ---------------------------------------------------------------------------
    -- Win32 API functions
    ---------------------------------------------------------------------------
    package Windows_Bindings is
        -- Skip the dependencies by not bringing in all of Win32.
        type LONG is new Interfaces.C.long;
        type DWORD is new Interfaces.C.unsigned_long;
        type HANDLE is new Interfaces.C.ptrdiff_t;
        type LPDWORD is access all DWORD;
        type BOOL is new Interfaces.C.int;
        type UINT is new Interfaces.C.unsigned;

        function To_DWORD is new Ada.Unchecked_Conversion (LONG, DWORD);

        INVALID_HANDLE_VALUE : constant       := -1;
        STD_INPUT_HANDLE     : constant DWORD := To_DWORD (-10);
        STD_OUTPUT_HANDLE    : constant DWORD := To_DWORD (-11);
        STD_ERROR_HANDLE     : constant DWORD := To_DWORD (-12);

        function GetStdHandle (Std_Handle : DWORD) return HANDLE;
        function GetConsoleMode (H : HANDLE; Mode : LPDWORD) return BOOL;
        function SetConsoleMode (H : HANDLE; dwMode : DWORD) return BOOL;

        CP_UTF8 : constant := 65_001;

        function GetConsoleCP return UINT;
        function GetConsoleOutputCP return UINT;

        function SetConsoleCP (wCodePageID : UINT) return BOOL;
        function SetConsoleOutputCP (wCodePageID : UINT) return BOOL;

        pragma Import (Stdcall, GetStdHandle, "GetStdHandle");
        pragma Import (Stdcall, GetConsoleMode, "GetConsoleMode");
        pragma Import (Stdcall, SetConsoleMode, "SetConsoleMode");

        pragma Import (Stdcall, GetConsoleCP, "GetConsoleCP");
        pragma Import (Stdcall, SetConsoleCP, "SetConsoleCP");
        pragma Import (Stdcall, GetConsoleOutputCP, "GetConsoleOutputCP");
        pragma Import (Stdcall, SetConsoleOutputCP, "SetConsoleOutputCP");

        --!pp off
        type Console_Input_Flags is
            (ENABLE_PROCESSED_INPUT,
             ENABLE_LINE_INPUT,
             ENABLE_ECHO_INPUT,
             ENABLE_WINDOW_INPUT,
             ENABLE_MOUSE_INPUT,
             ENABLE_INSERT_MODE,
             ENABLE_QUICK_EDIT_MODE,
             ENABLE_EXTENDED_FLAGS,
             ENABLE_AUTO_POSITION,
             ENABLE_VIRTUAL_TERMINAL_INPUT);

        for Console_Input_Flags use
            (ENABLE_PROCESSED_INPUT        => 16#0001#,
             ENABLE_LINE_INPUT             => 16#0002#,
             ENABLE_ECHO_INPUT             => 16#0004#,
             ENABLE_WINDOW_INPUT           => 16#0008#,
             ENABLE_MOUSE_INPUT            => 16#0010#,
             ENABLE_INSERT_MODE            => 16#0020#,
             ENABLE_QUICK_EDIT_MODE        => 16#0040#,
             ENABLE_EXTENDED_FLAGS         => 16#0080#,
             ENABLE_AUTO_POSITION          => 16#0100#,
             ENABLE_VIRTUAL_TERMINAL_INPUT => 16#0200#);

        type Console_Output_Flags is
            (ENABLE_PROCESSED_OUTPUT,
             ENABLE_WRAP_AT_EOL_OUTPUT,
             ENABLE_VIRTUAL_TERMINAL_PROCESSING,
             DISABLE_NEWLINE_AUTO_RETURN,
             ENABLE_LVB_GRID_WORLDWIDE);

        for Console_Output_Flags use
            (ENABLE_PROCESSED_OUTPUT            => 16#0001#,
             ENABLE_WRAP_AT_EOL_OUTPUT          => 16#0002#,
             ENABLE_VIRTUAL_TERMINAL_PROCESSING => 16#0004#,
             DISABLE_NEWLINE_AUTO_RETURN        => 16#0008#,
             ENABLE_LVB_GRID_WORLDWIDE          => 16#0010#);
        --!pp on

        pragma Warnings (Off, "bits of *unused");
        type Console_Input_Mode is array (Console_Input_Flags) of Boolean with
            Pack,
            Size => 32;
        type Console_Output_Mode is array (Console_Output_Flags) of Boolean with
            Pack,
            Size => 32;
        pragma Warnings (On, "bits of *unused");

        function To_Console_Input_Mode is new Ada.Unchecked_Conversion (DWORD, Console_Input_Mode);
        function To_Console_Output_Mode is new Ada.Unchecked_Conversion (DWORD, Console_Output_Mode);
        function To_DWORD is new Ada.Unchecked_Conversion (Console_Input_Mode, DWORD);
        function To_DWORD is new Ada.Unchecked_Conversion (Console_Output_Mode, DWORD);
    end Windows_Bindings;
    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    package Win renames Windows_Bindings;
    use type Win.BOOL;
    use type Win.HANDLE;
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Original settings
    ---------------------------------------------------------------------------
    Input_Settings, Output_Settings, Error_Settings : Win.DWORD;
    Original_Input_CP, Original_Output_CP           : Win.UINT;

    type Input_Stream is record
        Handle   : Win.HANDLE             := Win.INVALID_HANDLE_VALUE;
        Settings : Win.Console_Input_Mode := Win.To_Console_Input_Mode (0);
    end record;

    type Output_Stream is record
        Handle   : Win.HANDLE              := Win.INVALID_HANDLE_VALUE;
        Settings : Win.Console_Output_Mode := Win.To_Console_Output_Mode (0);
    end record;

    procedure Apply (Input : Input_Stream) is
    begin
        if Win.SetConsoleMode(Input.Handle, Win.To_DWORD(Input.Settings)) = 0 then
            AIO.Put_Line ("Unable to change console modes.");
        end if;
    end Apply;

    procedure Apply (Output : Output_Stream) is
    begin
        if Win.SetConsoleMode(Output.Handle, Win.To_DWORD(Output.Settings)) = 0 then
            AIO.Put_Line ("Unable to change console modes.");
        end if;
    end Apply;

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input             : Input_Stream;
    Std_Output, Std_Error : Output_Stream;

    function Load_Std_Settings return Boolean is
        Input_DWORD, Output_DWORD, Error_DWORD : aliased Win.DWORD;
    begin
        if Win.GetConsoleMode (Std_Input.Handle, Input_DWORD'Unchecked_Access) = 0 then
            return False;
        end if;
        if Win.GetConsoleMode (Std_Output.Handle, Output_DWORD'Unchecked_Access) = 0 then
            return False;
        end if;
        if Win.GetConsoleMode (Std_Error.Handle, Error_DWORD'Unchecked_Access) = 0 then
            return False;
        end if;

        Std_Input.Settings  := Win.To_Console_Input_Mode (Input_DWORD);
        Std_Output.Settings := Win.To_Console_Output_Mode (Output_DWORD);
        Std_Error.Settings  := Win.To_Console_Output_Mode (Error_DWORD);

        return True;
    end Load_Std_Settings;

    function Enable_UTF8 return Boolean is
    begin
        return Win.SetConsoleCP (Win.CP_UTF8) /= 0 and then Win.SetConsoleOutputCP (Win.CP_UTF8) /= 0;
    end Enable_UTF8;

    function Init return Boolean is
        -- Initializes and captures the original settings for the terminal so they can
        -- be restored when the system is shutdown.
    begin
        Std_Output.Handle := Win.GetStdHandle (Win.STD_OUTPUT_HANDLE);
        Std_Input.Handle  := Win.GetStdHandle (Win.STD_INPUT_HANDLE);
        Std_Error.Handle  := Win.GetStdHandle (Win.STD_ERROR_HANDLE);

        if Std_Output.Handle = Win.INVALID_HANDLE_VALUE or else Std_Input.Handle = Win.INVALID_HANDLE_VALUE
            or else Std_Error.Handle = Win.INVALID_HANDLE_VALUE then
            AIO.Put_Line ("Unable to get one or more of in/out/err handles.");
            return False;
        end if;

        if not Load_Std_Settings then
            return False;
        end if;

        -- Save the initial settings to be restored later.
        Input_Settings  := Win.To_DWORD (Std_Input.Settings);
        Output_Settings := Win.To_DWORD (Std_Output.Settings);
        Error_Settings  := Win.To_DWORD (Std_Error.Settings);

        Original_Input_CP  := Win.GetConsoleCP;
        Original_Output_CP := Win.GetConsoleOutputCP;

        if not Enable_UTF8 then
            AIO.Put_Line ("Unable to set UTF8 code page.");
            return False;
        end if;

        return True;
    end Init;

    procedure Shutdown is
    begin
        if Win.SetConsoleMode (Std_Input.Handle, Input_Settings) = 0
            or else Win.SetConsoleMode (Std_Output.Handle, Output_Settings) = 0
            or else Win.SetConsoleMode (Std_Error.Handle, Error_Settings) = 0 then
            AIO.Put_Line ("Unable to restore all terminal settings to originals.");
        end if;

        if Win.SetConsoleCP (Original_Input_CP) = 0 or else Win.SetConsoleOutputCP (Original_Output_CP) = 0 then
            AIO.Put_Line ("Unable to restore original terminal code page.");
        end if;
    end Shutdown;

    procedure Print_Capabilities is
    begin
        AIO.Put_Line ("Printing capabilities.");
    end Print_Capabilities;

    procedure Set (Setting : Input_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Echo =>
                Std_Input.Settings (Win.ENABLE_ECHO_INPUT) := Enabled;
                Apply(Std_Input);
            when Line_Input =>
                Std_Input.Settings (Win.ENABLE_LINE_INPUT) := Enabled;
                Apply(Std_Input);
        end case;
    end Set;

    procedure Set (Setting : Output_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Escape_Sequences =>
                Std_Output.Settings (Win.ENABLE_VIRTUAL_TERMINAL_PROCESSING) := Enabled;
                Std_Error.Settings (Win.ENABLE_VIRTUAL_TERMINAL_PROCESSING)  := Enabled;
                Std_Input.Settings (Win.ENABLE_VIRTUAL_TERMINAL_INPUT)       := Enabled;
                Apply(Std_Input);
                Apply(Std_Output);
                Apply(Std_Error);
        end case;
    end Set;

end Archaic_Terminal;
