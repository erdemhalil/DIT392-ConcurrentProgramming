## 1. Description ###
---
The program controls the flow of two trains running concurrently on the same track. Each train's logic rusn on its own thread making the two trains independent of each other.
To prevent collision and derailment (running out of track or switches being used wrongly), we placed sensors in the identified critical sections.

The first train to reach an intersection acquires its semaphore and holds it until leaving that specific intersection, both reaching and leaving are checked via the use of sensors. 
If the other train approaches the same intersection and tries to acquire the semaphore, it stops and waits until the semaphore can be acquired once again. This ensures that there is no more than one train at an intersection at a given time.

Stations have 2 tracks (top and bottom). When a train is reaching a station, it acquires the semaphore and unlocks the track. In case the other train is coming towards the station too, it takes the free track as the other one is occupied (the semaphore is acquired).

The middle of the map features two parallel sections. When two trains approach the section one after another going in the same direction, the first train to get to the corresponding sensor acquires the semaphore and takes the shorter path while the train behind is forced to take the other available track.

## 2. Placement of the sensors ##
---
Sensor were placed on the entrances/exits of intersections to handle possible collisions and stations to allow trains to stop and change direction [(see image)](image.pdf). 

The exact coordinate was done with little experimenting, making sure that there is just enough space for the braking distance so the train does not derail or enter part of the intersection it's not supposed to.

All in all, our solution uses 18 sensors.  
4 sensors for the two stations which feature 2 tracks each. (0, 1, 14, 15)  
4 sensors for the top intersection (in red) as it has 4 entrances/exits. (2, 3, 4, 5)  
4 sensors for the mid intersection (in green) as it has 4 entrances/exits. (6, 7, 8, 9)  
4 sensors for the bot intersection (in blue) as it has 4 entrances/exits. (10, 11, 12, 13)  
Additional 2 sensors to enable smooth trainflow in the middle track (in purple). (16, 17)  

## 3. Choice of critical sections ###
---
We identified 3 types of critical sections on the map [(see image)](image.pdf):
1. Stations
2. Intersections
3. Middle (overtake) section

There are 2 stations where the trains are supposed to stop for 1-2s and then move in the opposite direction.
There are 3 critical intersections where 2 tracks meet which makes collisions possible:
1. Top intersection (red)
2. Mid intersection (green)
3. Bot intersection (blue)

Last but not least, as per the requirements, we identified the parallel track in the middle of the map (purple) that allows overtaking for faster trains.


## 4. Maximum train speed and the reason for it ##
---
We have noticed that at speeds over 15, the trains either derailed at the stations, or collided in the intersections because the sensor reaction and braking distance were longer than anticipated.
We had to choose between changing the location of every single sensor which might have had another side effects or stick with max speed of 15. Due to simplicity and time constraints, we went with the latter solution.

## 5. How you tested your solution ##
---
Our solution was manually tested. After having figured out the maximum speed, we began testing with most values in combination [0|15]. Most of the test cases were roughly formulated as such:
1. Train 1 has the same speed as Train 2 -> e.g. [8, 8] [15, 15] [4, 4]
2. Train 1 is slightly faster than Train 2 and otherwise -> e.g. [15, 10] [6, 9] [3, 1]
3. Train 1 is much faster than Train 2 and otherwise -> e.g [15, 3] [12, 4] [1, 11]

After testing for a few hours, we concluded that the testing phase was exhaustive enough as no derailments or collisions were observed during that time.

## 6. How to run ##

To build the solution, you can simply run the following command from the root directory of the project. This will place the class files in the bin folder.:

```
make all
``` 


You can then run the solution with:

```
java -cp bin Main Lab1.map 5 15
```