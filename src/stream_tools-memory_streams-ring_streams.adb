pragma Ada_2012;
package body Stream_Tools.Memory_Streams.Ring_Streams is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Ring_Stream; Item : out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Ring_Stream; Item : Ada.Streams.Stream_Element_Array)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Stream : in out Ring_Stream) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "End_Of_File unimplemented");
      return raise Program_Error with "Unimplemented function End_Of_File";
   end End_Of_File;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in out Ring_Stream; Hard : Boolean := False) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Reset unimplemented");
      raise Program_Error with "Unimplemented procedure Reset";
   end Reset;

end Stream_Tools.Memory_Streams.Ring_Streams;
