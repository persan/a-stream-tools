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

with Stream_Tools.Timed_Streams.Output;
with Stream_Tools.Timed_Streams.Input;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
with Ada.Real_Time;
with GNAT.IO; use GNAT.IO;
procedure Stream_Tools.Tests.Test_Timed_Streams is
   use Ada.Real_Time;
   O : aliased Stream_Tools.Timed_Streams.Output.Timed_Output_Stream;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   O.Create ("test");
   Integer'Write (O'Access, 1);
   delay 0.2;
   Integer'Write (O'Access, 2);
   delay 0.2;
   Integer'Write (O'Access, 3);
   O.Close;
   declare
      I : aliased Stream_Tools.Timed_Streams.Input.Timed_Input_Stream;
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Data : Integer := 0;
   begin
      I.Open ("test");
      Put_Line (To_Duration (Clock - Now)'Img & " : " & Data'Img);
      Integer'Read (I'Access, Data);
      Put_Line (To_Duration (Clock - Now)'Img & " : " & Data'Img);
      Integer'Read (I'Access, Data);
      Put_Line (To_Duration (Clock - Now)'Img & " : " & Data'Img);
      Integer'Read (I'Access, Data);
      Put_Line (To_Duration (Clock - Now)'Img & " : " & Data'Img);
      I.Close;
   end;

end Stream_Tools.Tests.Test_Timed_Streams;
