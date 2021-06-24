with Ada.Text_IO;
with Interfaces.C;
with System;

package body Archaic_Terminal is
    package AIO renames Ada.Text_IO;

    ---------------------------------------------------------------------------
    -- Interfacing with C
    ---------------------------------------------------------------------------
    -- Crash course in how this works.
    -- https://en.wikibooks.org/wiki/Ada_Programming/Types/access#Access_vs._System.Address
    type BOOL is new Interfaces.C.int;
    type FD is new Interfaces.C.int;
    type FILE_Ptr is new System.Address;

    function fileno (Stream : FILE_Ptr) return FD with
        Import     => True,
        Convention => C;

    function isatty (File_Descriptor : FD) return BOOL with
        Import     => True,
        Convention => C;

    stdin  : aliased FILE_Ptr;
    stdout : aliased FILE_Ptr;
    stderr : aliased FILE_Ptr;

    pragma Import (C, stdin, "stdin");
    pragma Import (C, stdout, "stdout");
    pragma Import (C, stderr, "stderr");

    NCCS : constant := 32;
    type tcflag_t is new Interfaces.C.unsigned;
    type cc_t is new Interfaces.C.unsigned_char;
    type speed_t is new Interfaces.C.unsigned;
    type cc_array is array (Natural range 0 .. NCCS - 1) of cc_t;

    --!pp off
    type c_lflag_t is (ISIG,
                       ICANON,
                       XCASE,
                       ECHO,
                       ECHOE,
                       ECHOK,
                       ECHONL,
                       NOFLSH,
                       TOSTOP,
                       ECHOCTL,
                       ECHOPRT,
                       ECHOKE,
                       FLUSHO,
                       PENDIN);

    for c_lflag_t use
      (ISIG    => 16#0000001#,
       ICANON  => 16#0000002#,
       XCASE   => 16#0000004#,
       ECHO    => 16#0000010#,
       ECHOE   => 16#0000020#,
       ECHOK   => 16#0000040#,
       ECHONL  => 16#0000100#,
       NOFLSH  => 16#0000200#,
       TOSTOP  => 16#0000400#,
       ECHOCTL => 16#0001000#,
       ECHOPRT => 16#0002000#,
       ECHOKE  => 16#0004000#,
       FLUSHO  => 16#0010000#,
       PENDIN  => 16#0040000#
      );
    --!pp on

    pragma Warnings (Off, "bits of *unused");
    type Local_Flags is array (c_lflag_t) of Boolean with
        Pack,
        Size => 32;
    pragma Warnings (On, "bits of *unused");

    type Termios is record
        c_iflag  : tcflag_t;
        c_oflag  : tcflag_t;
        c_cflag  : tcflag_t;
        c_lflag  : Local_Flags;
        c_line   : cc_t;
        c_cc     : cc_array;
        c_ispeed : speed_t;
        c_ospeed : speed_t;
    end record with
        Convention => C;

    function tcgetattr (File_Descriptor : FD; Terminal : System.Address) return BOOL with
        Import     => True,
        Convention => C;

    type Application_Time is
        (TCSANOW,   -- immediate effect
         TCSADRAIN, -- after all output written
         TCSAFLUSH  -- like drain, except input received as well
    );
    for Application_Time use (TCSANOW => 0, TCSADRAIN => 1, TCSAFLUSH => 2);

    function tcsetattr
        (File_Descriptor : FD; Effect_Time : Application_Time; Terminal : System.Address) return BOOL with
        Import     => True,
        Convention => C;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------

    type IOType is (Input, Output, Error);

    type IOStream is record
        File            : FILE_Ptr;
        File_Descriptor : FD;
        Settings        : aliased Termios;
    end record;

    ---------------------------------------------------------------------------
    -- Original settings
    ---------------------------------------------------------------------------
    Original_Input_Setting, Original_Output_Setting, Original_Error_Setting : Termios;

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input, Std_Output, Std_Error : IOStream;

    function Load_Terminal (File_Descriptor : FD; Terminal : not null access Termios) return Boolean is
    begin
        if isatty (File_Descriptor) = 0 then
            return False;
        end if;
        if tcgetattr (File_Descriptor, Terminal.all'Address) /= 0 then
            return False;
        end if;
        return True;
    end Load_Terminal;

    function Make_Handle (Stream_Type : IOType; Handle : out IOStream) return Boolean is
    begin
        case Stream_Type is
            when Input =>
                Handle.File := stdin;
            when Output =>
                Handle.File := stdout;
            when Error =>
                Handle.File := stderr;
        end case;

        Handle.File_Descriptor := fileno (Handle.File);
        return Load_Terminal (Handle.File_Descriptor, Handle.Settings'Unchecked_Access);
    end Make_Handle;

    procedure Require_Settings_Change (Stream : IOStream) with
        Pre => isatty (Stream.File_Descriptor) /= 0
    is
    begin
        if tcsetattr (Stream.File_Descriptor, TCSANOW, Stream.Settings'Address) /= 0 then
            AIO.Put_Line ("Unable to change settings.");
        end if;
    end Require_Settings_Change;

    function Init return Boolean is
        -- Initializes and captures the original settings for the terminal so they can
        -- be restored when the system is shutdown.
    begin
        if not Make_Handle (Input, Std_Input) or else not Make_Handle (Output, Std_Output)
            or else not Make_Handle (Error, Std_Error) then
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

    procedure Set (Setting : Input_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Echo =>
                Std_Input.Settings.c_lflag (ECHO) := Enabled;
            when Line_Input =>
                Std_Input.Settings.c_lflag (ICANON) := Enabled;
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

    procedure Print_Capabilities is
    begin
        null;
    end Print_Capabilities;
end Archaic_Terminal;
