package Assignment2;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

public class Gym implements Runnable
{
	private static final int GYM_SIZE = 30;
	private static final int GYM_REGISTERED_CLIENTS = 1500;
	private Map<WeightPlateSize,Integer> noOfWeightPlates;
	private Set<Integer> clients;
	private ExecutorService executorService;

	// various semaphores
	static Semaphore mutex=new Semaphore(1);

	static Semaphore [] appartus = new Semaphore [8];
	static {
    	for(int i = 0; i < 8; i++) {
        	appartus[i] = new Semaphore(5);
    	}
	}

	static Semaphore small = new Semaphore(110);
	static Semaphore medium = new Semaphore(90);
	static Semaphore large = new Semaphore(75);


	public Gym() {
		this.clients=new HashSet<Integer>();
		this.noOfWeightPlates=new HashMap<WeightPlateSize, Integer>();

		noOfWeightPlates.put(WeightPlateSize.LARGE_10KG, 75);
		noOfWeightPlates.put(WeightPlateSize.MEDIUM_5KG, 90);
		noOfWeightPlates.put(WeightPlateSize.SMALL_3KG, 110);


		int i = 0;
		while (i < GYM_SIZE) {
			clients.add(new Random().nextInt(GYM_REGISTERED_CLIENTS));
			i++;
		}
	}

	public void run() {
		executorService = Executors.newFixedThreadPool(GYM_SIZE);

		for(int client:clients) {
			executorService.execute(new Runnable() {
				public void run()
				{
					Client newClient = Client.generateRandom(client, noOfWeightPlates);
					try
					{
						newClient.completeRoutine();
					}
					catch(InterruptedException error)
					{
						error.printStackTrace();
					}
				}
			});
		}
		executorService.shutdown();
	}

}
