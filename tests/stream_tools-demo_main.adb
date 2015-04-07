-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2015 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Stream_Tools.Bufferd_Streams;
with GNAT.Formatted_String; use GNAT.Formatted_String;
with System;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
with Interfaces.C;
procedure Stream_Tools.Demo_Main is

   package Unsigned_Text_Io is new Ada.Text_IO.Modular_IO (Interfaces.C.unsigned);
   function "&" is new Mod_Format (Interfaces.C.unsigned, Unsigned_Text_Io.Put);

   procedure P2;
   procedure P2 is
      P          : aliased Stream_Tools.Bufferd_Streams.Bufferd_Stream (8, System.Default_Priority);
      task T1 is
         entry Start;
      end T1;
      task T2 is
         entry Start;
      end T2;

      task body T1 is
      begin
         accept Start;
         for I in 1 .. 10 loop
            Integer'Write (P'Access, I);
            delay 0.1;
         end loop;
         Integer'Write (P'Access, 2);

         Short_Short_Integer'Write (P'Access, 2);
         Short_Short_Integer'Write (P'Access, 3);
         Short_Short_Integer'Write (P'Access, 4);
         Short_Short_Integer'Write (P'Access, -1);

         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);

         Short_Short_Integer'Write (P'Access, -1);
      end T1;
      task body T2 is
         D : Interfaces.C.unsigned;
      begin
         accept Start;
         for I in 1 .. 13 loop
            Interfaces.C.unsigned'Read (P'Access, D);
            Ada.Text_IO.Put_Line (-(+"%2d -> % 08x" & I & D));
            delay 0.01;
         end loop;
      end T2;
   begin
      T1.Start;
      T2.Start;
   end P2;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   P2;
end Stream_Tools.Demo_Main;
