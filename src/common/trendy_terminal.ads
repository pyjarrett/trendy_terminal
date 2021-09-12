-- Trendy Terminal defines a known environment in which to perform input/output.
-- Failure to meet these requirements results in an failed initialization.
--
-- The requirements:
-- 1. UTF-8
-- 2. VT100 terminal escape sequences.
--
-- The base package provides platform-specific environment setup, and the basic
-- read/write commands on top of which to build functionality.
package Trendy_Terminal is

    -- A position on screen.  Due to scrolling it is possible for these values
    -- to be negative.
    type Cursor_Position is record
        Row : Integer;
        Col : Integer;
    end record;

end Trendy_Terminal;
