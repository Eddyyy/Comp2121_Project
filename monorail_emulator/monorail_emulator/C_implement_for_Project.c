#include <stdio.h>
#define MAX_STATION 10
#define MAX_CHAR 10
int main() {
	int Max_num, i, a;
	char **str;
	int *time_interval;
	Max_num = 0;
	printf("Please type the maximum number of stations:");
	scanf("%d", &Max_num);
	printf("\n");
	if (Max_num > MAX_STATION) printf("The maximum number of stations are 10.\n");
	str = calloc(Max_num, sizeof(char *));
	time_interval = calloc(Max_num, sizeof(int));
	for (i = 0; i < Max_num; i++) str[i] = calloc(MAX_CHAR, sizeof(char));
	for (i = 0; i < Max_num; i++) {
		printf("Please type the name of %d stations", i);
		scanf("%s", &str[i]);
		printf("\n");
	}

	for (i = 0; i < Max_num; i++) {
		if (i = Max_num) {
			a = 1;
		} else {
			a = i + 1; 
		}
		printf("The time from Station %d to Station %d is:", i, a);
		scanf("%d", &time_interval[i]);
	}
	while(TRUE) {
		if (Press = PB0);
		if (Press = PB1);
		if (Press = # );
		
	}
}

void motor_turn_on()
void motor_turn_off()