#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HASH_SIZE 10

typedef struct Node {
	char* key;
	int value;
	struct Node* next;
} Node;

typedef struct HashMap {
	Node* buckets[HASH_SIZE];
} HashMap;

unsigned int l5_hash(const char* key) {
	unsigned int hash = 0;
	for (int i = 0; i < strlen(key); i++) {
		hash = (hash * 31) + key[i];
	}
	return hash % HASH_SIZE;
}

Node* l5_createNode(const char* key, int value) {
	Node* newNode = (Node*)malloc(sizeof(Node));
	newNode->key = strdup(key);
	newNode->value = value;
	newNode->next = NULL;
	return newNode;
}

void l5_insert(HashMap* map, const char* key, int value) {
	unsigned int index = l5_hash(key);
	Node* newNode = l5_createNode(key, value);

	if (map->buckets[index] == NULL) {
		map->buckets[index] = newNode;
	} else {
		Node* current = map->buckets[index];
		while (current->next != NULL) {
			current = current->next;
		}
		current->next = newNode;
	}
}

int l5_get(HashMap* map, const char* key) {
	unsigned int index = l5_hash(key);
	Node* current = map->buckets[index];

	while (current != NULL) {
		if (strcmp(current->key, key) == 0) {
			return current->value;
		}
		current = current->next;
	}

	return -1; // Key not found
}
