with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with Trendy_Terminal.Input;
with Trendy_Terminal.VT100;

package body Trendy_Terminal is

    package AIO renames Ada.Text_IO;
    use all type ASU.Unbounded_String;

    ---------------------------------------------------------------------------
    -- Win32 API functions
    --
    -- https://docs.microsoft.com/en-us/windows/console/classic-vs-vt
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

        function GetLastError return DWORD;

        CP_UTF8 : constant := 65_001;

        function GetConsoleCP return UINT;
        function GetConsoleOutputCP return UINT;

        function SetConsoleCP (wCodePageID : UINT) return BOOL;
        function SetConsoleOutputCP (wCodePageID : UINT) return BOOL;

        pragma Import (Stdcall, GetStdHandle, "GetStdHandle");
        pragma Import (Stdcall, GetConsoleMode, "GetConsoleMode");
        pragma Import (Stdcall, SetConsoleMode, "SetConsoleMode");
        pragma Import (Stdcall, GetLastError, "GetLastError");

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

        type LPVOID is new Interfaces.C.Strings.chars_ptr;
        type LPCVOID is new Interfaces.C.Strings.chars_ptr;
        type LPOVERLAPPED is new Interfaces.C.ptrdiff_t;

        function WriteFile(hFile : HANDLE; lpBuffer : LPCVOID; BytesToWrite : DWORD;
                           NumBytesWritten : LPDWORD; Overlapped : LPOVERLAPPED) return BOOL;
        pragma Import (Stdcall, WriteFile, "WriteFile");
    end Windows_Bindings;
    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    package Win renames Windows_Bindings;
    use type Win.BOOL;
    use type Win.HANDLE;

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

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input             : Input_Stream;
    Std_Output, Std_Error : Output_Stream;

    procedure Write_Terminal(C : Character) is
        S       : constant String := (1 => C);
        C_Array : aliased Interfaces.C.char_array := Interfaces.C.To_C(S);
        Native  : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.To_Chars_Ptr(C_array'Unchecked_Access);
        Written : aliased Win.DWORD;
    begin
        if Win.WriteFile (Std_Output.Handle, Win.LPCVOID(Native), S'Length, Written'Unchecked_Access, 0) = 0 then
            AIO.Put_Line ("Write failed.");
        end if;
        Interfaces.C.Strings.Free(Native);
    end Write_Terminal;

    procedure Write_Terminal(S : String) is
        Native : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String(S);
        Written : aliased Win.DWORD;
    begin
        if Win.WriteFile (Std_Output.Handle, Win.LPCVOID(Native), S'Length, Written'Unchecked_Access, 0) = 0 then
            AIO.Put_Line ("Write failed.");
        end if;
        Interfaces.C.Strings.Free(Native);
    end Write_Terminal;

    procedure Write_Terminal_Line(S : String) renames AIO.Put_Line;

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
    begin
        Std_Output.Handle := Win.GetStdHandle (Win.STD_OUTPUT_HANDLE);
        Std_Input.Handle  := Win.GetStdHandle (Win.STD_INPUT_HANDLE);
        Std_Error.Handle  := Win.GetStdHandle (Win.STD_ERROR_HANDLE);

        if Std_Output.Handle = Win.INVALID_HANDLE_VALUE or else Std_Input.Handle = Win.INVALID_HANDLE_VALUE
            or else Std_Error.Handle = Win.INVALID_HANDLE_VALUE then
            Write_Terminal_Line ("Unable to get one or more of in/out/err handles.");
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
            Write_Terminal_Line ("Unable to set UTF8 code page.");
            return False;
        end if;

        return True;
    end Init;

    procedure Shutdown is
    begin
        if Win.SetConsoleMode (Std_Input.Handle, Input_Settings) = 0
            or else Win.SetConsoleMode (Std_Output.Handle, Output_Settings) = 0
            or else Win.SetConsoleMode (Std_Error.Handle, Error_Settings) = 0 then
            Write_Terminal_Line ("Unable to restore all terminal settings to originals.");
        end if;

        if Win.SetConsoleCP (Original_Input_CP) = 0 or else Win.SetConsoleOutputCP (Original_Output_CP) = 0 then
            Write_Terminal_Line ("Unable to restore original terminal code page.");
        end if;
    end Shutdown;

    procedure Apply (Input : Input_Stream) is
    begin
        if Win.SetConsoleMode(Input.Handle, Win.To_DWORD(Input.Settings)) = 0 then
            Write_Terminal_Line ("Unable to change console modes: ERROR#" & Win.GetLastError'Image);
        end if;
    end Apply;

    procedure Apply (Output : Output_Stream) is
    begin
        if Win.SetConsoleMode(Output.Handle, Win.To_DWORD(Output.Settings)) = 0 then
            Write_Terminal_Line ("Unable to change console modes: ERROR# " & Win.GetLastError'Image);
        end if;
    end Apply;

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

    function ReadConsoleA (I               : Win.HANDLE;
                           Buffer          : Win.LPVOID;
                           Buffer_Size     : Win.DWORD;
                           Characters_Read : Win.LPDWORD;
                           Console_Control : Interfaces.C.ptrdiff_t) return Win.BOOL;
    pragma Import (Stdcall, ReadConsoleA, "ReadConsoleA");

    -- The input stream might have existing inputs, and it may be necessary to
    -- discard these.  A use case would be clearing the input buffer to get
    -- VT100 sequences.
    procedure Clear_Input_Buffer is
        Buffer_Size  : constant := 1024;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Chars_Read   : aliased Win.DWORD;
        Result       : Win.BOOL;
        use all type Interfaces.C.size_t;
        use all type Win.DWORD;
    begin
        -- Put something into the buffer to ensure it won't block.
        -- It'd be better to peek than do this, but that might fail on named
        -- pipes for inputs and this is just a simple, but hacky way of doing it.
        VT100.Report_Cursor_Position;
        loop
            Result := ReadConsoleA (
                Std_Input.Handle,
                Win.LPVOID(Interfaces.C.Strings.To_Chars_Ptr(Buffer'Unchecked_Access)),
                Buffer_Size,
                Chars_Read'Unchecked_Access,
                0);
            exit when Chars_Read < Buffer_Size;
        end loop;
        pragma Unreferenced (Result);
    end Clear_Input_Buffer;

    function Get_Cursor_Position return Cursor_Position is
    begin
        loop
            Clear_Input_Buffer;
            VT100.Report_Cursor_Position;
            declare
                Result : constant String := Get_Input;
                Semicolon_Index : constant Natural := Ada.Strings.Fixed.Index(Result, ";", 1);
                Row : Integer := 1;
                Col : Integer := 1;
            begin
                -- The cursor position is reported as
                -- ESC [ ROW ; COL R

                -- May throw on bad parse.
                Row := Integer'Value(Result(3 .. Semicolon_Index - 1));
                Col := Integer'Value(Result(Semicolon_Index + 1 .. Result'Length - 1));

                return Cursor_Position'(Row => Row, Col => Col);
            exception
                -- Bad parse due to existing input on the line.
                when Constraint_Error =>
                    null;
            end;
        end loop;
    end Get_Cursor_Position;

    -- Gets an entire input line from one keypress.  E.g. all the characters
    -- received for a controlling keypress, such as an arrow key.
    function Get_Input return String is
        Buffer_Size  : constant := 512;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Chars_Read   : aliased Win.DWORD;
        use all type Interfaces.C.size_t;
    begin
        if ReadConsoleA (Std_Input.Handle, Win.LPVOID(Interfaces.C.Strings.To_Chars_Ptr(Buffer'Unchecked_Access)),
                         Buffer_Size, Chars_Read'Unchecked_Access, 0) /= 0 then
            return Interfaces.C.To_Ada(Buffer(1 .. Interfaces.C.size_t(Chars_Read) + 1));
        else
            return "";
        end if;
    end Get_Input;

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
