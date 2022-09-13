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
      First : Boolean := True;
   begin
      for I of Item loop
         if not First then
            Append (Ret, " ");
         end if;
         First := False;
         Append (Ret, Image (I));
      end loop;
      return To_String (Ret);
   end Image;

end Stream_Tools.Tests;
