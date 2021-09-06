with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces.C.Strings;
with System;
with Trendy_Terminal.Input;
with Trendy_Terminal.VT100;

package body Trendy_Terminal is
    package AIO renames Ada.Text_IO;
    use all type ASU.Unbounded_String;

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

    function Make_Handle (File : File_Ptr; Handle : out IOStream) return Boolean is
    begin
        Handle.File := File;
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
        if not Make_Handle (stdin, Std_Input) or else not Make_Handle (stdout, Std_Output)
            or else not Make_Handle (stderr, Std_Error) then
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

    procedure Put (C : Character) is
    begin
        Ada.Text_IO.Put (C);
    end Put;

    procedure Put (S : String) is
    begin
        Ada.Text_IO.Put (S);
    end Put;

    procedure Put_Line (S : String) is
    begin
        Ada.Text_IO.Put (S);
    end Put_Line;

    procedure Print_Capabilities is
    begin
        null;
    end Print_Capabilities;

    type VOIDP is new Interfaces.C.Strings.chars_ptr;

    -- char *fgets(char *s, int size, FILE *stream);
    -- read (FD, void*, size_t) return ssize_t;
    -- constexpr size_t kBufferSize = 1024;
    -- char input[kBufferSize];
    -- const auto bytes = read (fileno(stdin), (void*)input, kBufferSize);
    -- function C_Read (S : Interfaces.C.char_ptr, Size : Integer; Stream : FILE_Ptr)
    function C_Read (File_Descriptor : FD;
                     Buffer          : VOIDP;
                     Buffer_Size     : Integer) return Integer
        with Import => True,
             Convention => C;

    function Get_Cursor_Position return Cursor_Position is
    begin
        VT100.Report_Cursor_Position;

        declare
            -- The cursor position is reported as
            -- ESC [ ROW ; COL R
            Result : constant String := Get_Input;
            Semicolon_Index : constant Natural := Ada.Strings.Fixed.Index(Result, ";", 1);
            Row : constant Integer := Integer'Value(Result(3 .. Semicolon_Index - 1));
            Col : constant Integer := Integer'Value(Result(Semicolon_Index + 1 .. Result'Length - 1));
        begin
            return Cursor_Position'(Row => Row, Col => Col);
        end;
    end Get_Cursor_Position;

    -- Gets a collection of input at the same time as a single string.
    --
    -- This provides unambiguous inputs for single character presses which
    -- generate multiple character presses, such as "Up arrow".
    function Get_Input return String is
        Buffer_Size  : constant := 512;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Buffer_Ptr   : constant VOIDP := VOIDP(Interfaces.C.Strings.To_Chars_Ptr
            (Buffer'Unchecked_Access));
        Chars_Read   : Integer;
        use all type Interfaces.C.size_t;
    begin
        Chars_Read := C_Read (Std_Input.File_Descriptor, Buffer_Ptr, Buffer_Size);
        if Chars_Read /= 0 then
            return Interfaces.C.To_Ada(Buffer(1 .. Interfaces.C.size_t(Chars_Read) + 1));
        else
            return "";
        end if;
    end Get_Input;

    -- TODO: Move the following two functions into a common place since they're
    -- duplicated between Windows and Linux.

    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    function Get_Line(Format_Fn     : Format_Function := null;
                      Completion_Fn : Completion_Function := null) return String
    is
        package TTI renames Trendy_Terminal.Input;
        use all type Interfaces.C.int;

        Input_Line : ASU.Unbounded_String;
        Input      : Interfaces.C.int;
        Key_Enter  : constant := 13;
        KM         : constant TTI.Key_Maps.Map := TTI.Make_Key_Map;
        MK         : constant TTI.Inverse_Key_Maps.Map := TTI.Make_Key_Lookup_Map;
        L          : Trendy_Terminal.Input.Line;
        Line_Pos   : constant Cursor_Position := Get_Cursor_Position;
        Edit_Pos   : Cursor_Position := Line_Pos;

        -- Prints an updated input line at the given starting position.
        procedure Print_Line (Pos : Cursor_Position; S : String) is
        begin
            VT100.Position_Cursor (Pos);
            VT100.Clear_Line;
            Write_Terminal (S);
        end Print_Line;
    begin
        loop
            -- Clear anything which has been printed and then print the current
            -- state of the line.

            if Format_Fn /= null then
                Print_Line (Line_Pos, Format_Fn (TTI.Current (L)));
            else
                Print_Line (Line_Pos, TTI.Current (L));
            end if;

            Edit_Pos.Row := Line_Pos.Row;
            Edit_Pos.Col := TTI.Cursor_Index(L);
            VT100.Position_Cursor (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Get_Input);

            if MK(Key_Left) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Left);
            elsif MK(Key_Right) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Right);
            elsif MK(Key_Backspace) = Input_Line then
                TTI.Backspace (L);
            elsif MK(Key_Delete) = Input_Line then
                TTI.Delete (L);
            elsif MK(Key_Tab) = Input_Line then
                -- Do tab completion on the line
                if Completion_Fn /= null then
                    -- Adjust the cursor position?
                    null;
                end if;
            elsif ASU.Length (Input_Line) = 1 then
                Input := Character'Pos(ASU.Element(Input_Line, 1));

                -- Line has been finished.
                if Input = Key_Enter then
                    return TTI.Current(L);
                end if;
            end if;

            -- Actual text was inserted.
            -- TODO: Maybe add a "replace" mode?
            if not KM.Contains (Input_Line) then
                TTI.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Get_Line;
    
    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    function Debug_Get_Line (Format_Fn     : Format_Function := null;
                             Completion_Fn : Completion_Function := null;
                             Debug_Fn      : Format_Function := null) return String
    is
        package TTI renames Trendy_Terminal.Input;
        use all type Interfaces.C.int;

        Input_Line : ASU.Unbounded_String;
        Input      : Interfaces.C.int;
        Key_Enter  : constant := 13;
        KM         : constant TTI.Key_Maps.Map := TTI.Make_Key_Map;
        MK         : constant TTI.Inverse_Key_Maps.Map := TTI.Make_Key_Lookup_Map;
        L          : Trendy_Terminal.Input.Line;
        Line_Pos   : constant Cursor_Position := Get_Cursor_Position;
        Debug_Pos  : Cursor_Position := Line_Pos;
        Edit_Pos   : Cursor_Position := Line_Pos;

        function Format_Line (S : String) return String is
            use Ada.Characters.Latin_1;
        begin
            return Quotation & S & Quotation;
        end Format_Line;

        -- Prints an updated input line at the given starting position.
        procedure Print_Line (Pos : Cursor_Position; S : String) is
        begin
            VT100.Position_Cursor (Pos);
            VT100.Clear_Line;
            Write_Terminal (S);
        end Print_Line;
    begin
        pragma Unreferenced (Format_Fn, Completion_Fn);

        Debug_Pos.Row := Debug_Pos.Row - 1;

        loop
            if Debug_Fn /= null then
                Print_Line (Debug_Pos, Debug_Fn (TTI.Current (L)));
            else
                Print_Line (Debug_Pos, "Cursor @ " & TTI.Cursor_Index(L)'Image);
            end if;

            -- Clear anything which has been printed and then print the current
            -- state of the line.
            Print_Line (Line_Pos, Format_Line (TTI.Current (L)));

            Edit_Pos.Row := Line_Pos.Row;
            Edit_Pos.Col := TTI.Cursor_Index(L) + 1; -- add 1 for the " around the debug line
            VT100.Position_Cursor (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Get_Input);

            if MK(Key_Left) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Left);
            elsif MK(Key_Right) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Right);
            elsif MK(Key_Backspace) = Input_Line then
                TTI.Backspace (L);
            elsif MK(Key_Delete) = Input_Line then
                TTI.Delete (L);
            elsif ASU.Length (Input_Line) = 1 then
                Input := Character'Pos(ASU.Element(Input_Line, 1));

                -- Line has been finished.
                if Input = Key_Enter then
                    return TTI.Current(L);
                end if;
            end if;

            -- Actual text was inserted.
            -- TODO: Maybe add a "replace" mode?
            if not KM.Contains (Input_Line) then
                TTI.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Debug_Get_Line;
end Trendy_Terminal;

