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

with Stream_Tools.Bufferd_Streams;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with Stream_Tools.Stream_Element_Array_Image;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Assertions; use Ada.Assertions;
with System; use System;

procedure Stream_Tools.Tests.Main is
   B : aliased Stream_Tools.Bufferd_Streams.Bufferd_Stream (6, System.Default_Priority);
   I : Ada.Streams.Stream_Element_Array (1 .. 4) := (others => 0);

begin
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   Short_Short_Integer'Write (B'Access, 1);
   Short_Short_Integer'Write (B'Access, 2);
   Short_Short_Integer'Write (B'Access, 3);
   Short_Short_Integer'Write (B'Access, 4);
   Short_Short_Integer'Write (B'Access, 5);
   Short_Short_Integer'Write (B'Access, 6);

   I := (others => 0);
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line ("          :" & Stream_Element_Array_Image (I));
--     Assert (I = (1, 2, 3, 4), "");
   Put_Line ("---------------------");
   Short_Short_Integer'Write (B'Access, 7);
   Short_Short_Integer'Write (B'Access, 8);

   I := (others => 0);
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line ("          :" & Stream_Element_Array_Image (I));
--     Assert (I = (5, 6, 7, 8), "");
   Put_Line ("---------------------");

   Short_Short_Integer'Write (B'Access, 1);
   Short_Short_Integer'Write (B'Access, 2);
   Short_Short_Integer'Write (B'Access, 3);
   Short_Short_Integer'Write (B'Access, 4);
   I := (others => 0);
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line ("          :" & Stream_Element_Array_Image (I));
--     Assert (I = (1, 2, 3, 4), "");

   Short_Short_Integer'Write (B'Access, 1);
   Short_Short_Integer'Write (B'Access, 2);
   Short_Short_Integer'Write (B'Access, 3);
   Short_Short_Integer'Write (B'Access, 4);
   I := (others => 0);
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line ("          :" & Stream_Element_Array_Image (I));
   Assert (I = (1, 2, 3, 4), "");
   I := (1, 2, 3, 4);
   --  Stream_Element_Array'Write (B'Access, Stream_Element_Array'(1, 2, 3, 4));
   Stream_Element_Array'Write (B'Access, I);

end Stream_Tools.Tests.Main;
