with Ada.Strings.Unbounded;

package body Stream_Tools.Tests is
   use Ada.Streams;
   use Ada.Strings.Unbounded;
   S : constant array (Stream_Element'(0) .. Stream_Element'(15)) of Character := "0123456789ABCDEF";
   -----------
   -- Image --
   -----------
   function Image (Item : Ada.Streams.Stream_Element) return String is
   begin
      return S (Item / 16) & S (Item mod 16);
   end Image;

   -----------
   -- Image --
   -----------
   function Image (Item : Ada.Streams.Stream_Element_Array) return String is
      Ret : Unbounded_String;
   begin
      for I of Item loop
         Append (Ret, Image (I));
         Append (Ret, " ");
      end loop;
      return Slice (Ret, 1, Length (Ret) - 1);
   end Image;

end Stream_Tools.Tests;
