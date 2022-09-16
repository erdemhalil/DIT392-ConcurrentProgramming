import TSim.*;
import java.util.concurrent.Semaphore;


public class Lab1 {
    
  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    Train train1 = new Train(1, speed1, 2);
    Train train2 = new Train(2, speed2, 1);


    train1.start();
    train2.start();

  }
  
}

class Train extends Thread {
	public TSimInterface tsi = TSimInterface.getInstance();

	// Semaphores starting with 0 permits due to the train occupying the track when the simulator is started.
    static Semaphore semTopIntersection = new Semaphore(1);
    static Semaphore semTopTopStation = new Semaphore(0);
    static Semaphore semTopBotStation = new Semaphore(1);
    static Semaphore semMidTrack = new Semaphore(1);
    static Semaphore semBotTopStation = new Semaphore(0);
    static Semaphore semBotBotStation = new Semaphore(1);
    static Semaphore semMidIntersection = new Semaphore(1);
    static Semaphore semBotIntersection = new Semaphore(1);
    
    private boolean usedTop = false;

	/*
		Mapping indices to sensors.
		Index one and two reference the x and y coordinate respectively, while the third value in each array represents the
		direction towards the critical section that the sensor is observing.
	 */
    static int[][] sensors = new int[][] {
    	new int[] {16, 3, 1}, // 0
    	new int[] {16, 5, 1}, // 1
    	new int[] {6, 6, 2}, // 2
    	new int[] {9, 5, 2}, // 3
    	new int[] {10, 8, 1}, // 4
    	new int[] {11, 7, 1}, // 5
    	new int[] {14, 7, 2}, // 6
    	new int[] {15, 8, 2}, // 7
    	new int[] {12, 9, 1}, // 8
    	new int[] {13, 10, 1}, // 9
    	new int[] {7, 9, 2}, // 10
    	new int[] {6, 10, 2}, // 11
    	new int[] {4, 13, 1}, // 12
    	new int[] {6, 11, 1}, // 13
    	new int[] {16, 11, 2}, // 14
    	new int[] {16, 13, 2}, // 15
    	new int[] {1, 9, 1}, // 16
    	new int[] {19, 9, 2}, // 17
    };

	// Representing each switch on the track as an array of arrays.
    static int[][] switches = new int[][] {
    	new int[] {17, 7}, // 0
    	new int[] {15, 9}, // 1
    	new int[] {4, 9}, // 2
    	new int[] {3, 11}, // 3
    };
    
	public int trainId;
	public int trainSpeed;
	public int direction; // Direction 1 means train is heading north and direction 2 means train is heading south.
	
	public Train(int trainId, int trainSpeed, int direction) {
		this.trainId = trainId;
		this.trainSpeed = trainSpeed;
		this.direction = direction; // Direction 1 means the train is heading north and direction 2 means the train is heading south.
	}

	// Method to make the train stop at the station. The direction of the train is reversed, and the train speed is multiplied by -1 to make the train go in the opposite direction.
	public void stopAtStation() throws CommandException, InterruptedException {
		tsi.setSpeed(this.trainId, 0);
		this.direction = this.direction == 1 ? 2 : 1;
		Thread.sleep(1000+(20*Math.abs(this.trainSpeed)));
		this.trainSpeed = this.trainSpeed* -1;
		tsi.setSpeed(this.trainId, this.trainSpeed);
	}

	// Method checks if the train passing a given sensor is heading away from the critical sections.
	public boolean checkSensor(SensorEvent e, int[] sensor) throws CommandException, InterruptedException {
		if (e.getXpos() == sensor[0] && e.getYpos() == sensor[1] && e.getStatus() == 1 && sensor[2] != this.direction) {
			return true;
		}
		return false;
	}

	// Method checks if the train passing a given sensor is heading towards the critical sections.
	public boolean checkBlockingSensor(SensorEvent e, int[] sensor) throws CommandException, InterruptedException {
		if (e.getXpos() == sensor[0] && e.getYpos() == sensor[1] && e.getStatus() == 1 && sensor[2] == this.direction) {
			return true;
		}
		return false;
	}
	
	/* 	Method used to lock a critical section of the track or to lock a track leading to a station by acquiring a semaphore.
		When the semaphore is acquired the train speed is set to the original value. */
	public void lockTrack(Semaphore semaphore) throws CommandException, InterruptedException {
		tsi.setSpeed(this.trainId, 0);
		semaphore.acquire();
		tsi.setSpeed(this.trainId, this.trainSpeed);
	}
	
	/* Method that locks a critical section leading up to a switch, method makes sure the train stops while trying to acquiring the semaphore.
	   When the semaphore is acquired the switch is set and train speed is set to the original value. */
	public void lockSwitchTrack(Semaphore semaphore, int x, int y, int direction) throws CommandException, InterruptedException {
		tsi.setSpeed(this.trainId, 0);
		semaphore.acquire();		
		tsi.setSwitch(x, y, direction);
		tsi.setSpeed(this.trainId, this.trainSpeed); 
	}
	
	
	public void run() {
	    try {
	    	tsi.setSpeed(this.trainId, this.trainSpeed);
	    	while (true) {
	    		SensorEvent e = tsi.getSensor(this.trainId);
	    		
				// Checks if the train passed sensors at the station heading towards the station.
	    		if (checkBlockingSensor(e, sensors[0]) || checkBlockingSensor(e, sensors[1]) || checkBlockingSensor(e, sensors[14]) || checkBlockingSensor(e, sensors[15])) {
	    			stopAtStation(); 
	    		}

				// Locks the TopIntersection so that only one train can pass at a time.
	    		if (checkBlockingSensor(e, sensors[2]) || checkBlockingSensor(e, sensors[3])  || checkBlockingSensor(e, sensors[4]) || checkBlockingSensor(e, sensors[5])) {
	    			lockTrack(semTopIntersection);	    				
	    		}

				// Release semaphore after the TopIntersection is passed.
	    		if (checkSensor(e, sensors[2]) || checkSensor(e, sensors[3])  || checkSensor(e, sensors[4]) || checkSensor(e, sensors[5])) {
	    			semTopIntersection.release();
	    		}

				// Sets switch to avoid derailment and release TopTopStation semaphore due to leaving the station track.
	    		if (checkBlockingSensor(e, sensors[6])) {
	    			lockSwitchTrack(semMidIntersection, switches[0][0], switches[0][1], 2);
	    			semTopTopStation.release();
	    		}

				// Sets switch to avoid derailment and release TopBotStation semaphore due to leaving the station track.
				if (checkBlockingSensor(e, sensors[7])) {
	    			lockSwitchTrack(semMidIntersection, switches[0][0], switches[0][1], 1);
	    			semTopBotStation.release();
	    		}

				// Release semaphore after the MidIntersection is exited.
	    		if (checkSensor(e, sensors[6]) || checkSensor(e, sensors[7]) || checkSensor(e, sensors[8]) || checkSensor(e, sensors[9])) {
	    			semMidIntersection.release();
	    		}

				// Tries to acquire MidTrack semaphore, if the successful train uses the fast middle track, otherwise, it gets directed to the parallel one.
	    		if (checkBlockingSensor(e, sensors[16])) {
	    			if (semMidTrack.tryAcquire()) {
	    				tsi.setSwitch(switches[2][0], switches[2][1], 1);
	    				usedTop = true; // flag to know what semaphore to release on exit.
	    			} else {
	    				tsi.setSwitch(switches[2][0], switches[2][1], 2);
	    				usedTop = false;
	    			} 
	    		}

				// Tries to acquire MidTrack semaphore, if the successful train uses the fast middle track, otherwise, it gets directed to the parallel one.
				if (checkBlockingSensor(e, sensors[17])) {
	    			if (semMidTrack.tryAcquire()) {
	    				tsi.setSwitch(switches[1][0], switches[1][1], 2);
	    				usedTop = true;
	    			} else {
	    				tsi.setSwitch(switches[1][0], switches[1][1], 1);
	    				usedTop = false;
	    			} 
	    		}
				// If the fast middle track was used the usedTop flag is true and the semaphore is realised on upon exiting the critical middle section.
	    		if (checkSensor(e, sensors[16]) || checkSensor(e, sensors[17])) {
	    			if (usedTop) {
	    				semMidTrack.release();
	    			}
	    		}

				// If TopTopStation is not occupied the train defaults to it otherwise chooses the TopBotStation.
	    		if (checkBlockingSensor(e, sensors[8])) {
	    			if (semTopTopStation.tryAcquire()) {
	    				lockSwitchTrack(semMidIntersection, switches[0][0], switches[0][1], 2);
	    			} else if (semTopBotStation.tryAcquire()) {
	    				lockSwitchTrack(semMidIntersection, switches[0][0], switches[0][1], 1);
	    			} else {
	    				tsi.setSpeed(this.trainId, 0);
	    			}
    				tsi.setSwitch(switches[1][0], switches[1][1], 2);
	    		}

				// If TopTopStation is not occupied the train defaults to it otherwise chooses the TopBotStation.
				if (checkBlockingSensor(e, sensors[9])) {
	    			if (semTopTopStation.tryAcquire()) {
	    				lockSwitchTrack(semMidIntersection, switches[0][0], switches[0][1], 2);
	    			} else if (semTopBotStation.tryAcquire()) {
	    				lockSwitchTrack(semMidIntersection, switches[0][0], switches[0][1], 1);
	    			} else {
	    				tsi.setSpeed(this.trainId, 0);
	    			}
    				tsi.setSwitch(switches[1][0], switches[1][1], 1);
	    		}

				// If BotTopStation is not occupied the train defaults to it otherwise chooses the BotBotStation.
				if (checkBlockingSensor(e, sensors[10])) {
	    			lockSwitchTrack(semBotIntersection, switches[2][0], switches[2][1], 1);
	    			if (semBotTopStation.availablePermits() > 0) {
		    			lockSwitchTrack(semBotTopStation, switches[3][0], switches[3][1], 1);
	    			} else if (semBotBotStation.availablePermits() > 0) {
		    			lockSwitchTrack(semBotBotStation, switches[3][0], switches[3][1], 2);
	    			} else {
	    				tsi.setSpeed(this.trainId, 0);
	    			}
	    		}

				// If BotTopStation is not occupied the train defaults to it otherwise chooses the BotBotStation.
				if (checkBlockingSensor(e, sensors[11])) {
	    			lockSwitchTrack(semBotIntersection, switches[2][0], switches[2][1], 2);
	    			if (semBotTopStation.tryAcquire()) {
	    				tsi.setSwitch(switches[3][0], switches[3][1], 1);
	    			} else if (semBotBotStation.tryAcquire()) {
	    				tsi.setSwitch(switches[3][0], switches[3][1], 2);
	    			} else {
	    				tsi.setSpeed(this.trainId, 0);
	    			}
	    			
	    		}

				// Release semaphore after the critical section is exited.
				if (checkSensor(e, sensors[10]) || checkSensor(e, sensors[11]) || checkSensor(e, sensors[12]) || checkSensor(e, sensors[13])) {
	    			semBotIntersection.release();
	    		}

				// Set switch to avoid derailment, release semaphore for BotBotTrack, and lock intersection while crossing.
	    		if (checkBlockingSensor(e, sensors[12])) {
	    			semBotBotStation.release();
	    			lockSwitchTrack(semBotIntersection, switches[2][0], switches[2][1], 1);
    				tsi.setSwitch(switches[3][0], switches[3][1], 2);
	    		}

				// Set switch to avoid derailment, release semaphore for BotTopTrack, and lock intersection while crossing.
	    		if (checkBlockingSensor(e, sensors[13])) {
	    			semBotTopStation.release();
	    			lockSwitchTrack(semBotIntersection, switches[2][0], switches[2][1], 1);
    				tsi.setSwitch(switches[3][0], switches[3][1], 1);
	    		}
	    	}
	    }	    		
	    catch (CommandException | InterruptedException e) {    	
	    	e.printStackTrace();    // or only e.getMessage() for the error
	    	System.exit(1);
	    }
	    	}
	
}
