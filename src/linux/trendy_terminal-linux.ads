-------------------------------------------------------------------------------
-- Copyright 2021, The Trendy Terminal Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

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
    type cc_array is array (Natural range 0 .. NCCS - 1) of cc_t with Convention => C;

    --!pp off
    type c_lflag_bit is (ISIG,
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
                       PENDIN,
                       IEXTEN,
                       EXTPROC)
       with Size => tcflag_t'Size;

    for c_lflag_bit use
      (ISIG    => 8#0000001#,
       ICANON  => 8#0000002#,
       XCASE   => 8#0000004#,
       ECHO    => 8#0000010#,
       ECHOE   => 8#0000020#,
       ECHOK   => 8#0000040#,
       ECHONL  => 8#0000100#,
       NOFLSH  => 8#0000200#,
       TOSTOP  => 8#0000400#,
       ECHOCTL => 8#0001000#,
       ECHOPRT => 8#0002000#,
       ECHOKE  => 8#0004000#,
       FLUSHO  => 8#0010000#,
       PENDIN  => 8#0040000#,
       IEXTEN  => 8#0100000#,
       EXTPROC => 8#0200000#
      );
    --!pp on

    pragma Warnings (Off, "bits of *unused");
    type c_lflag_t is array (c_lflag_bit) of Boolean with
        Pack,
        Size => tcflag_t'Size;
    pragma Warnings (On, "bits of *unused");

    type Termios is record
        c_iflag  : tcflag_t;
        c_oflag  : tcflag_t;
        c_cflag  : tcflag_t;
        c_lflag  : c_lflag_t;
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
