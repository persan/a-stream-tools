-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2016 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
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

with GNAT.Memory_Dump;
with GNAT.IO;
package body Stream_Tools.Stream_Debug_Readers is
   use Ada.Streams;

   procedure Display (Item : Ada.Streams.Stream_Element_Array) is
   begin
      GNAT.Memory_Dump.Dump (Item'Address, Item'Length, GNAT.Memory_Dump.None);
   end Display;

   ------------------------
   -- Stream_Reader_Task --
   ------------------------

   task body Stream_Reader_Task is
      In_Buffer      : Stream_Element_Array (1 .. 1);
      Last           : Stream_Element_Offset;
      Display_Buffer : Stream_Element_Array (1 .. 16);
      Cursor         : Stream_Element_Offset := Display_Buffer'First;

   begin
      Read_Loop :  loop
         S.Read (In_Buffer, Last);
         if Last = In_Buffer'Last then
            Display_Buffer (Cursor) :=  In_Buffer (In_Buffer'First);
            Cursor := Cursor + 1;
            if Cursor > Display_Buffer'Last then
               Display (Display_Buffer);
               Cursor := Display_Buffer'First;
            end if;
         else
            exit Read_Loop;
         end if;
      end loop Read_Loop;
      Display (Display_Buffer (Display_Buffer'First .. Cursor - 1));
   end Stream_Reader_Task;

   task body Text_Stream_Reader_Task is

      type Buffer_Type (Part : Boolean := False) is record
         case Part is
            when True  => As_Character            : Character;
            when False => As_Stream_Element_Array : Stream_Element_Array (1 .. 1);
         end case;
      end record with
        Unchecked_Union => True;

      In_Buffer      : Buffer_Type;
      Last           : Stream_Element_Offset;

      Display_Buffer : String (1 .. 256);
      Cursor         : Natural := Display_Buffer'First;
   begin
      Read_Loop :  loop
         S.Read (In_Buffer.As_Stream_Element_Array, Last);
         if Last = In_Buffer.As_Stream_Element_Array'Last then
            Display_Buffer (Cursor) :=  (if Printable (In_Buffer.As_Character)
                                         then
                                            In_Buffer.As_Character
                                         else
                                        '.');
            Cursor := Cursor + 1;
            if Cursor > Display_Buffer'Last then
               GNAT.IO.Put (Display_Buffer);
               Cursor := Display_Buffer'First;
            end if;
         else
            exit Read_Loop;
         end if;
      end loop Read_Loop;
      GNAT.IO.Put (Display_Buffer (Display_Buffer'First .. Cursor - 1));
   end Text_Stream_Reader_Task;

end Stream_Tools.Stream_Debug_Readers;
