package Assignment2;
import java.util.*;

public enum ApparatusType {
	LEGPRESSMACHINE, BARBELL, HACKSQUATMACHINE, LEGEXTENSIONMACHINE, LEGCURLMACHINE, LATPULLDOWNMACHINE, PECDECKMACHINE, CABLECROSSOVERMACHINE;

	public static ApparatusType randomAppartusType() {
		int appartatusNum = new Random().nextInt(8);
		ApparatusType appartus = ApparatusType.values()[appartatusNum];
		return appartus;
	}
}
