with Stream_Tools.Bufferd_Streams;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with Stream_Tools.Stream_Element_Array_Image;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
procedure Stream_Tools.Tests.Main is
   B : aliased Stream_Tools.Bufferd_Streams.Bufferd_Stream (6);
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
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line (Stream_Element_Array_Image (I));
   Short_Short_Integer'Write (B'Access, 7);
   Short_Short_Integer'Write (B'Access, 8);
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line (Stream_Element_Array_Image (I));

   Short_Short_Integer'Write (B'Access, 1);
   Short_Short_Integer'Write (B'Access, 2);
   Short_Short_Integer'Write (B'Access, 3);
   Short_Short_Integer'Write (B'Access, 4);
   Ada.Streams.Stream_Element_Array'Read (B'Access, I);
   Put_Line (Stream_Element_Array_Image (I));

end Stream_Tools.Tests.Main;
