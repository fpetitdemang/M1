using System;
using System.Threading;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Http;
using System.Runtime.Remoting.Channels.Tcp;

namespace RemotingSamples {
  public class Client {
    
    public bool init = false;
    public static Thread thread1 = null;
    public static Thread thread2 = null;
    
    public static int Main(string [] args)
    {
        TcpChannel chan = new TcpChannel();
        ChannelServices.RegisterChannel(chan, true);
      Client c = new Client();  
      thread1 = new Thread(new ThreadStart(c.RunMe));
      thread2 = new Thread(new ThreadStart(c.RunMe)); 
      thread1.Start();
      thread2.Start();
      Console.Read();
      return 0;
    } 
    
    
    public void RunMe()
    {
        
      if (Thread.CurrentThread == thread1) {
        
        Console.WriteLine("Ceci est le thread un");
        HelloServer obj = (HelloServer)Activator.GetObject(typeof(HelloServer), "tcp://localhost:8089/SayHello");
        for (int i = 0; i < 100; i++) {
          Console.WriteLine(obj.CountMe() + " -  partir du thread 1 "); 
          Thread.Sleep(0);
        }  
      }  
      else if (Thread.CurrentThread == thread2) {
       // TcpChannel chan = new TcpChannel();
        //ChannelServices.RegisterChannel(chan, true);
        Console.WriteLine("Ceci est le thread deux");
        HelloServer obj = (HelloServer)Activator.GetObject(typeof(HelloServer), "tcp://localhost:8089/SayHello");
        for (int i = 0; i < 100; i++) {
          Console.WriteLine(obj.CountMe() + " -  partir du thread 2 "); 
          Thread.Sleep(0);
        }  
      }  
    }
  }
}
