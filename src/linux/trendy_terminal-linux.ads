with Interfaces.C.Strings;
with System;

package Trendy_Terminal.Linux is

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

end Trendy_Terminal.Linux;
