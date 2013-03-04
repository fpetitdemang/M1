using System;
using System.Runtime.Remoting;

public class ServiceClass : MarshalByRefObject{

   public ServiceClass() {
      Console.WriteLine("ServiceClass created.");
   }

   public string VoidCall(){
      Console.WriteLine("VoidCall called.");
      return "You are calling the void call on the ServiceClass.";
   }

   public int GetServiceCode(){
      return this.GetHashCode();
   }

   public string TimeConsumingRemoteCall(){
      Console.WriteLine("TimeConsumingRemoteCall called.");
   
      for(int i = 0; i < 20000; i++){
         Console.Write("Counting: " + i.ToString());
         Console.Write("\r");
      }
      return "This is a time-consuming call.";
   }
}