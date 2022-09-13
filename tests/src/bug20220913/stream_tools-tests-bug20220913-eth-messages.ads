with Ada.Streams;
with System.Unsigned_Types;
private with Ada.Tags;
private with Stream_Tools.Tests.BUG20220913_Low_Defs;

package Stream_Tools.Tests.BUG20220913.Eth.Messages is
   type Root_Message_Type is abstract tagged private
     with
       Input'Class => Input,
         Output'Class => Output;

   function Input (S : not null access Ada.Streams.Root_Stream_Type'Class)
                   return Root_Message_Type'Class;

   procedure Output (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : Root_Message_Type'Class);

   function Length (Item : Root_Message_Type)
                    return System.Unsigned_Types.Unsigned is abstract;

   function Constructor (Params : not null access Ada.Streams.Root_Stream_Type'Class)
                         return Root_Message_Type is abstract;

   use type System.Unsigned_Types.Unsigned; -- Since its used in all childpackages.

private
   type Root_Message_Type is abstract tagged record
      Header    : Stream_Tools.Tests.BUG20220913_Low_Defs.Non_Streamable_Link_Header;
   end record;

   procedure Register
     (Message_Id : System.Unsigned_Types.Unsigned; Tag : Ada.Tags.Tag);

end Stream_Tools.Tests.BUG20220913.Eth.Messages;
