using System;
using System.Reflection;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Messaging;
using System.Runtime.Remoting.Channels;
using System.Threading;

public class RemotingDelegates : MarshalByRefObject{

    /// <summary>
    /// L'evt e permet d'avertir les threads en attente qu'un événement s'est produit : ici que l'appel asynchrone s'est terminé
    /// </summary>
    public static ManualResetEvent e; 
    public delegate string RemoteSyncDelegate();
   
    public delegate string RemoteAsyncDelegate();

    // This is the call that the AsyncCallBack delegate references.
    [OneWayAttribute]
   public void OurRemoteAsyncCallBack(IAsyncResult ar){
    // ASyncResult encapsule le résultat issu d'un appel asynchrone ; 
    // AsyncDelegate permet de récupérer l'objet delegate sur lequel l'appel asynchrone a été invoqué.
    // ligne suivante, on ne fait donc que récupérer dans del le delegate sur lequel l'appel asynchrone a été effectué 
      RemoteAsyncDelegate del = (RemoteAsyncDelegate)((AsyncResult)ar).AsyncDelegate;
      Console.WriteLine("\r\n**SUCCESS**: Result of the remote AsyncCallBack: "  + del.EndInvoke(ar) );
      
        // Signal the thread. Set : événement signalé, les threads en attente peuvent poursuivre
        e.Set();
        return;
   }

    public static void Main(string[] Args){

        // IMPORTANT: .NET Framework remoting does not remote
        // static members. This class must be an instance before
        // the callback from the asynchronous invocation can reach this client.
        RemotingDelegates HandlerInstance = new RemotingDelegates();
        HandlerInstance.Run();        
    }

    public void Run(){
        // Enable this and the e.WaitOne call at the bottom if you 
        // are going to make more than one asynchronous call.
        e = new ManualResetEvent(false); // false : evt non signalé

        Console.WriteLine("Remote synchronous and asynchronous delegates.");
        Console.WriteLine(new String('-',80));
        Console.WriteLine();

        // This is the only thing you must do in a remoting scenario
        // for either synchronous or asynchronous programming 
        // configuration.
        RemotingConfiguration.Configure("SyncAsync.exe.config", true);

        // The remaining steps are identical to single-
        // AppDomain programming. Sauf si on veut utiliser la ligne suivante, plus prudente qu'un new
       // ServiceClass obj = (ServiceClass)Activator.GetObject(typeof(ServiceClass), "tcp://localhost:8085/ServiceClass.rem");
        ServiceClass obj = new ServiceClass(); // attention, on récupère un proxy ...
       
        // This delegate is a remote synchronous delegate.
        RemoteSyncDelegate Remotesyncdel = new RemoteSyncDelegate(obj.VoidCall);
        
        // When invoked, program execution waits until the method returns.
        // This delegate can be passed to another application domain
        // to be used as a callback to the obj.VoidCall method.
        Console.WriteLine(Remotesyncdel());
        Console.WriteLine("Pause 1");
        Console.Read();
        // This delegate is an asynchronous delegate. Two delegates must 
        // be created. The first is the system-defined AsyncCallback 
        // delegate, which references the method that the remote type calls 
        // back when the remote method is done.

        AsyncCallback RemoteCallback = new AsyncCallback(this.OurRemoteAsyncCallBack);

        // Create the delegate to the remote method you want to use 
        // asynchronously.
        RemoteAsyncDelegate RemoteDel = new RemoteAsyncDelegate(obj.TimeConsumingRemoteCall);
        
        // Start the method call. Note that execution on this 
        // thread continues immediately without waiting for the return of 
        // the method call. 
        IAsyncResult RemAr = RemoteDel.BeginInvoke(RemoteCallback, null); // BeginInvoke est générée : prend d'abord les paramètres s'il y en a (ici : non, TimeConsumingRemoteCall ne prend pas de param), puis le callback, puis un objet qcq, qui peut être utile par exemple à passer des informations d'état)
        Console.WriteLine("Pause 2");
        Console.Read();
        // If you want to stop execution on this thread to 
        // wait for the return from this specific call, retrieve the 
        // IAsyncResult returned from the BeginIvoke call, obtain its 
        // WaitHandle, and pause the thread, such as the next line:
        // RemAr.AsyncWaitHandle.WaitOne();

        // To wait in general, if, for example, many asynchronous calls 
        // have been made and you want notification of any of them, or, 
        // like this example, because the application domain can be 
        // recycled before the callback can print the result to the 
        // console.
        //e.WaitOne();

   // This simulates some other work going on in this thread while the 
   // async call has not returned. 
   int count = 0;
   while(!RemAr.IsCompleted){
      Console.Write("\rNot completed: " + (++count).ToString());
      // Make sure the callback thread can invoke callback.
      Thread.Sleep(1);
   }
   Console.Read();
    }
} 
