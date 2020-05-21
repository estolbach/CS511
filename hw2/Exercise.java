package Assignment2;
import java.util.*;

public class Exercise
{
	private ApparatusType at;
	private Map<WeightPlateSize, Integer> weight;
	private int duration;

	public Exercise(ApparatusType at, Map<WeightPlateSize, Integer> weight, int duration) {
		this.at = at;
		this.weight = weight;
		this.duration = duration;
	}

	public static Exercise generateRandom(Map<WeightPlateSize, Integer> weight) {
		ApparatusType appType = ApparatusType.randomAppartusType();

		Map<WeightPlateSize, Integer> weightMap = new HashMap<WeightPlateSize, Integer>();

		for(WeightPlateSize weights : weight.keySet()) {
			weightMap.put(weights, new Random().nextInt(10));
		}

		return new Exercise(appType, weightMap, new Random().nextInt(350));
	}

	public ApparatusType getApparatusType()
	{
		return at;
	}

	public Map<WeightPlateSize, Integer> getWeights()
	{
		return weight;
	}

	public int getDuration()
	{
		return duration;
	}

	public String toString()
	{
		String str = "Apparatus Type: "+at.toString();
		str = str+"\nWeight Plates Size: "+weight.toString();
		str = str+"\nDuration: "+duration+"\n\n";

		return str;
	}
}
