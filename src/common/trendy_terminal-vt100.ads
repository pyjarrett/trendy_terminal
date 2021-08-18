
-- ECMA-48 (ISO 6429)
-- C0 (7-bit) control codes
-- C1 (8-bit) control codes
-- CSI = Control Sequence Introducer
-- https://vt100.net/
-- https://vt100.net/docs/vt510-rm/chapter4.html
-- https://en.wiktionary.org/wiki/Appendix:Control_characters
-- https://www.aivosto.com/articles/control-characters.html
-- https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
-- https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
package Trendy_Terminal.VT100 is

    procedure Cursor_Left;
    procedure Cursor_Right;

    procedure Erase;

    procedure Clear_Line;

end Trendy_Terminal.VT100;
