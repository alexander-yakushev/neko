package neko;

import android.util.Log;
import clojure.java.api.Clojure;
import clojure.lang.DalvikDynamicClassLoader;
import clojure.lang.Var;
import clojure.lang.IFn;

public class App extends android.app.Application {

    private static String TAG = "neko.App";
    public static App instance;

    @Override
    public void onCreate() {
        instance = this;
        DalvikDynamicClassLoader.setContext(this);
        // ClassLoader loader = Thread.currentThread().getContextClassLoader();
        // Log.d(TAG, "What's the loader! " + loader);
        // if (loader instanceof DalvikDynamicClassLoader) {
        //     Log.d(TAG, "INSIDE!");
        //     ((DalvikDynamicClassLoader)loader).initializeDynamicCompilation(this);
        // }
    }

    // This method is only necessary for asynchronous loading. Clojure is
    // perfectly capable of loading itself the first time anything from it is
    // called.
    public static void loadClojure() {
        IFn load = Clojure.var("clojure.core", "load");
        load.invoke("/neko/init");

        IFn init = Clojure.var("neko.init", "init");
        init.invoke(instance);
    }

    public static void loadAsynchronously(final String activityClass, final Runnable callback) {
        new Thread(Thread.currentThread().getThreadGroup(),
                   new Runnable(){
                       @Override
                       public void run() {
                           loadClojure();

                           try {
                               Class.forName(activityClass);
                           } catch (ClassNotFoundException e) {
                               Log.e(TAG, "Failed loading activity " + activityClass, e);
                           }

                           callback.run();
                       }
                   },
                   "ClojureLoadingThread",
                   1048576 // = 1MB, thread stack size in bytes
                   ).start();
    }

}
