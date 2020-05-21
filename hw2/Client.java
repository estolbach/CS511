package Assignment2;
import java.util.*;

public class Client
{
	private int id;
	private List<Exercise> routine;

	public Client(int id)
	{
		this.id=id;
		routine = new ArrayList<Exercise>();
	}

	public void addExercise(Exercise e)
	{
		routine.add(e);
	}

	public static Client generateRandom(int id, Map<WeightPlateSize, Integer> weight)
	{
		Client client = new Client(id);
		Random rand = new Random();

		int routine = rand.nextInt(15);
		int i = 0;
		while ( i < routine) {
			client.addExercise(Exercise.generateRandom(weight));
			i++;
		}

		return client;
	}



	public void completeRoutine() throws InterruptedException
	{
		for (int i = 0; i < routine.size(); i++)
		{
			Gym.mutex.acquire();

			System.out.println("ClientID Number: " +this.id+", Start Exercise Routine: \n"+ routine.get(i).toString());

			ApparatusType appartusType = routine.get(i).getApparatusType();

			switch(appartusType) {
				case LEGPRESSMACHINE:
					Gym.appartus[0].acquire();
					break;
				case BARBELL:
					Gym.appartus[1].acquire();
					break;
				case HACKSQUATMACHINE:
					Gym.appartus[2].acquire();
					break;
				case LEGEXTENSIONMACHINE:
					Gym.appartus[3].acquire();
					break;
				case LEGCURLMACHINE:
					Gym.appartus[4].acquire();
					break;
				case LATPULLDOWNMACHINE:
					Gym.appartus[5].acquire();
					break;
				case PECDECKMACHINE:
					Gym.appartus[6].acquire();
					break;
				case CABLECROSSOVERMACHINE:
					Gym.appartus[7].acquire();
					break;
			}

			for (WeightPlateSize weightPlate : routine.get(i).getWeights().keySet()) {
				if(weightPlate == WeightPlateSize.SMALL_3KG)
				{
				    for (int j = 0; j < routine.get(i).getWeights().get(weightPlate); j++)
				    {
				    	Gym.small.acquire();
				    }
				}
				if(weightPlate == WeightPlateSize.MEDIUM_5KG)
				{
					for (int j = 0; j < routine.get(i).getWeights().get(weightPlate); j++)
					{
						Gym.medium.acquire();
					}
				}
				if(weightPlate == WeightPlateSize.LARGE_10KG)
				{
					for (int j = 0; j < routine.get(i).getWeights().get(weightPlate); j++)
					{
						Gym.large.acquire();
					}
				}
			}

			Gym.mutex.release();
			Thread.sleep(routine.get(i).getDuration());

			System.out.println("ClientID Number: " +this.id+", Completed Exercise Routine: \n"+ routine.get(i).toString());

			switch(appartusType) {
				case LEGPRESSMACHINE:
					Gym.appartus[0].release();
					break;
				case BARBELL:
					Gym.appartus[1].release();
					break;
				case HACKSQUATMACHINE:
					Gym.appartus[2].release();
					break;
				case LEGEXTENSIONMACHINE:
					Gym.appartus[3].release();
					break;
				case LEGCURLMACHINE:
					Gym.appartus[4].release();
					break;
				case LATPULLDOWNMACHINE:
					Gym.appartus[5].release();
					break;
				case PECDECKMACHINE:
					Gym.appartus[6].release();
					break;
				case CABLECROSSOVERMACHINE:
					Gym.appartus[7].release();
					break;
			}

		    for(WeightPlateSize weightPlate: routine.get(i).getWeights().keySet())
		    {
		    	if(weightPlate == WeightPlateSize.SMALL_3KG)
		    	{
		    		for (int j = 0; j < routine.get(i).getWeights().get(weightPlate); j++)
		    		{
		    			Gym.small.release();
		    		}
		    	}
		    	if(weightPlate == WeightPlateSize.MEDIUM_5KG)
		    	{
		    		for (int j = 0; j < routine.get(i).getWeights().get(weightPlate); j++)
		    		{
		    			Gym.medium.release();
		    		}
		    	}
		    	if(weightPlate == WeightPlateSize.LARGE_10KG)
		    	{
		    		for (int j = 0; j < routine.get(i).getWeights().get(weightPlate); j++)
		    		{
		    			Gym.large.release();
		    		}
		    	}
		    }
		}
	}

	public String toString()
	{
		String str="ClientID Number: "+id+"\n";

		for(int i = 0; i < routine.size(); i++)
		{
			str = str+routine.get(i).toString();
		}

		return str;
	}
}
