
function Stream_Tools.Image
  (Item : Ada.Streams.Stream_Element_Array)
   return String
is
   use Ada.Streams;
   type Nibble is range 0 .. 15;
   type Narray_Array is array (1 .. Item'Length * 2) of Nibble;
   pragma Pack (Narray_Array);
   Map : constant array (Nibble) of Character := "0123456789ABCDEF";
   Src : Narray_Array;
   for Src'Address use Item'Address;
begin
   return Ret : String (Src'Range) := (others => '-') do
      for I in Src'Range loop
         Ret (I) := Map (Src (I));
      end loop;
   end return;
end Stream_Tools.Image;
