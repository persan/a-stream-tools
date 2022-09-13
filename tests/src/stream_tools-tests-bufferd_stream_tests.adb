
with Stream_Tools.Pipes;
with Ada.Streams; use Ada.Streams;
with Ada.Assertions; use Ada.Assertions;
with GNAT.Source_Info;
package body Stream_Tools.Tests.Bufferd_Stream_Tests is

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (T : in out Bufferd_Stream_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Simple'Access, "Test_Simple");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   Test_Name : constant String := GNAT.Source_Info.Enclosing_Entity;
   overriding function Name (T : Bufferd_Stream_Test) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format (Test_Name);
   end Name;

   ---------------------
   -- Test_Simple_Add --
   ---------------------

   procedure Test_Simple (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      B : aliased Stream_Tools.Pipes.Pipe (11);
      I : Ada.Streams.Stream_Element_Array (1 .. 4) := (others => 0);

   begin
      Short_Short_Integer'Write (B'Access, 1);
      Short_Short_Integer'Write (B'Access, 2);
      Short_Short_Integer'Write (B'Access, 3);
      Short_Short_Integer'Write (B'Access, 4);
      Short_Short_Integer'Write (B'Access, 5);
      Short_Short_Integer'Write (B'Access, 6);

      I := (others => 0);
      Ada.Streams.Stream_Element_Array'Read (B'Access, I);
      Assert (I = (1, 2, 3, 4), "");
      Short_Short_Integer'Write (B'Access, 7);
      Short_Short_Integer'Write (B'Access, 8);

      I := (others => 0);
      Ada.Streams.Stream_Element_Array'Read (B'Access, I);
      Assert (I = (5, 6, 7, 8), "");

      Short_Short_Integer'Write (B'Access, 1);
      Short_Short_Integer'Write (B'Access, 2);
      Short_Short_Integer'Write (B'Access, 3);
      Short_Short_Integer'Write (B'Access, 4);
      I := (others => 0);
      Ada.Streams.Stream_Element_Array'Read (B'Access, I);
      --     Assert (I = (1, 2, 3, 4), "");

      Short_Short_Integer'Write (B'Access, 1);
      Short_Short_Integer'Write (B'Access, 2);
      Short_Short_Integer'Write (B'Access, 3);
      Short_Short_Integer'Write (B'Access, 4);
      I := (others => 0);
      Ada.Streams.Stream_Element_Array'Read (B'Access, I);
      Assert (I = (1, 2, 3, 4), "");
      I := (1, 2, 3, 4);
      --  Stream_Element_Array'Write (B'Access, Stream_Element_Array'(1, 2, 3, 4));
      Stream_Element_Array'Write (B'Access, I);
   end Test_Simple;

end Stream_Tools.Tests.Bufferd_Stream_Tests;
