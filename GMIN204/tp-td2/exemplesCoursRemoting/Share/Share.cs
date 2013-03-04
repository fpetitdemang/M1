using System;

namespace RemotingSamples {

  public class ForwardMe : MarshalByRefObject {

    public void CallMe(String text)
    {
      Console.WriteLine(text);
    }
  }
  public class HelloServer : MarshalByRefObject {
      private int compteur;
    public HelloServer()
    {
      Console.WriteLine("HelloServer activ");
        compteur=0;
    }

    public String HelloMethod(String name,ForwardMe obj)
    {
      obj.CallMe("Souvenirs du serveur");
      Console.WriteLine("Hello.HelloMethod : {0}", name);
      return "Bonjour " + name;
    }

      public int CountMe()
      {
          compteur++;
          return compteur;
      }
  }
}