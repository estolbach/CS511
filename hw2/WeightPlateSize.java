package Assignment2;

import java.util.*;

public enum WeightPlateSize {
	SMALL_3KG , MEDIUM_5KG , LARGE_10KG;

	public static WeightPlateSize getWeight() {
		int weightSize = new Random().nextInt(3);
		WeightPlateSize weights = WeightPlateSize.values()[weightSize];
		return weights;
	}
}
