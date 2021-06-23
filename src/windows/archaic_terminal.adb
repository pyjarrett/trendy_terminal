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

    type IOStream is record
        Handle   : Win.HANDLE        := Win.INVALID_HANDLE_VALUE;
        Settings : aliased Win.DWORD := 0;
    end record;

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input, Std_Output, Std_Error                : IOStream;

    function Gather (Stream : in out IOStream) return Boolean is
    begin
        return Win.GetConsoleMode (Stream.Handle, Stream.Settings'Unchecked_Access) /= 0;
    end Gather;

    function Enable_UTF8 return Boolean is
    begin
        return Win.SetConsoleCP(Win.CP_UTF8) /= 0 and then Win.SetConsoleOutputCP(Win.CP_UTF8) /= 0;
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

        if not Gather (Std_Input) or else not Gather (Std_Output) or else not Gather (Std_Error) then
            AIO.Put_Line ("Unable to gather all standard streams.");
            return False;
        end if;

        -- Save the initial settings to be restored later.
        Input_Settings  := Std_Input.Settings;
        Output_Settings := Std_Output.Settings;
        Error_Settings  := Std_Error.Settings;

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
        null;
    end Set;

    procedure Set (Setting : Output_Setting; Enabled : Boolean) is
    begin
        null;
    end Set;

end Archaic_Terminal;
