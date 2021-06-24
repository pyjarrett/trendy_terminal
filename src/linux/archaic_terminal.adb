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
    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------

    function Init return Boolean is
      -- Initializes and captures the original settings for the terminal so they can
      -- be restored when the system is shutdown.
    begin
        AIO.Put_Line (fileno (stdin)'Image);
        AIO.Put_Line (fileno (stdout)'Image);
        AIO.Put_Line (fileno (stderr)'Image);

        AIO.Put_Line (isatty (fileno (stdin))'Image);
        AIO.Put_Line (isatty (fileno (stdout))'Image);
        AIO.Put_Line (isatty (fileno (stderr))'Image);

        return True;
    end Init;

    procedure Shutdown is
    begin
        null;
    end Shutdown;

    procedure Set (Setting : Input_Setting; Enabled : Boolean) is
    begin
        pragma Unreferenced (Enabled);
        case Setting is
            when Echo =>
                null;
            when Line_Input =>
                null;
        end case;
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
