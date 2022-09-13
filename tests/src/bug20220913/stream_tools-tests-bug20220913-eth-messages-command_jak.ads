with Stream_Tools.Tests.BUG20220913_Low_Defs; use Stream_Tools.Tests.BUG20220913_Low_Defs;

package Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak is

   Id : constant := 16#06#;

   type Command_Jak_Message is new Root_Message_Type with record
      Data : Jp_List_Distribution_Acknowledge_T;
   end record with
     Write => Write;

   procedure Write (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : Command_Jak_Message);
   overriding function Constructor (S : not null access Ada.Streams.Root_Stream_Type'Class) return Command_Jak_Message;

   overriding function Length (Item : Command_Jak_Message) return System.Unsigned_Types.Unsigned is
     (Stream_Tools.Tests.BUG20220913_Low_Defs.Jp_List_Distribution_Acknowledge_T'Size / Ada.Streams.Stream_Element'Size);

end Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak;
