#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define MAX 1024
#define TOP 20
#define INF 999999

int V = 0;
int time = 0;
int NServers = 0;
int Cable = 0;
int Topology = 0;
int Networks = 0;
int *card;

struct node{

    int serv;
    int src;
    int val;
    int cost;
    struct node* next;

};

struct graph{

    int v;
    struct node** arr;

};

struct AdjList {
    struct node* head;
};

struct MinHeapNode {
    int v;
    int key;
};

struct MinHeap {
    int size;
    int capacity;
    int* pos;
    struct MinHeapNode** array;
};

void printSolution(int **dist,struct graph *g){
    int i, j;
    printf ("The following matrix shows the shortest distances"
            " between every pair of vertices \n");
    for ( i = 1; i < g->v; i++){
        for ( j = 1; j < g->v; j++){
            if (dist[i][j] == INF)
                printf("%5s", "INF");
            else
                printf ("%5d", dist[i][j]);
        }
        printf("\n");
    }
}

int min(int a,int b){
    return(a<b?a:b);
}

void printServers(struct graph *s){

    int i = 1;
    for(;i<s->v;i++){
        if(s->arr[i]->serv){
            printf("___SERVER %d: ",s->arr[i]->src);
        }else{
            printf("___EQUIPMENT %d: ",s->arr[i]->src);
        }
        printf("\n");
    }

}


/** ------------------------------ FASE 0 (CHECK) ---------------------------- **/


struct graph* createGraph(int v){

    int i;
    struct graph* temp =(struct graph*)malloc(sizeof(struct graph));
    temp->v=v;
    for(i=0;i<v;i++){
        temp->arr=(struct node**)malloc(sizeof(struct node*)*v);
    }
    for(i=0;i<v;i++){
        temp->arr[i]=NULL;
    }
    return temp;

}

void addEdge(struct graph* g,int u,int v,int c){

    struct node* temp =(struct node*)malloc(sizeof(struct node));
    struct node* temp2 =(struct node*)malloc(sizeof(struct node));
    temp->serv = temp2->serv = 0;
    temp->src = u;
    temp2->src = v;
    temp->val = v;
    temp2->val = u;
    temp->cost = temp2->cost = c;
    temp->next = g->arr[u];
    temp2->next = g->arr[v];
    g->arr[u] = temp;
    g->arr[v] = temp2;

}

/** -------------------------------FASE 1 (CHECK) ---------------------------- **/


void ArPointsRec(struct graph * g,int node,int* isVisited,int* des,int* parent,int* low,int* ap){

    struct node* temp=NULL;
    int children=0;
    isVisited[node]=1;
    des[node]=low[node]=++time;
    temp = g->arr[node];
    while(temp!=NULL){
       if(!isVisited[temp->val]){
          children++;
          parent[temp->val]=node;
          ArPointsRec(g,temp->val,isVisited,des,parent,low,ap);
          low[node]= min(low[node],low[temp->val]);
          if(parent[node]==-1 && children>1){
            ap[node]=1;
          }
          if(parent[node]!=-1 && des[node]<=low[temp->val]){
            ap[node]=1;
          }
        }
        else if(temp->val!=parent[node]){
            low[node]=min(low[node],des[temp->val]);
        }
        temp= temp->next;
    }

}

struct graph *ArPoints(struct graph* g, int *n){

    int i,j;
    int* des = (int*)malloc(sizeof(int)*g->v);
    int* isVisited = (int*)malloc(sizeof(int)*g->v);
    int* parent = (int*)malloc(sizeof(int)*g->v);
    int* low = (int*)malloc(sizeof(int)*g->v);
    int* ap = (int*)malloc(sizeof(int)*g->v);
    struct graph* servers = NULL;

    for(i=0;i<g->v;i++){
        parent[i]=-1;
        isVisited[i]=0;
        ap[i]=0;
    }

    for(i=0;i<g->v;i++){
        if(isVisited[i]==0){
            ArPointsRec(g,i,isVisited,des,parent,low,ap);
        }
    }

    for(i=0;i<g->v;i++){
        if(ap[i]==1){
            g->arr[i]->serv = 1;
            NServers++;
        }
    }

    if(NServers > 1){
        j = 0;
        servers =(struct graph*)malloc(sizeof(struct graph));
        servers->v = NServers;
        servers->arr = (struct node**)malloc(sizeof(struct node*)*NServers);
        for(i=0;i<g->v;i++){
            if(ap[i]==1){
                servers->arr[j] = g->arr[i];
                servers->arr[j]->src = i;
                n[i] = i;
                j++;
            }
        }
    }

    return servers;

}

int **countNetworks(int **dist, struct graph *g){

    int i,j;
    int c = 1;
    int **net = (int **)malloc(sizeof(int *)*g->v);
    card = (int *)calloc(g->v,sizeof(int));

    for(i=0;i<g->v;i++){
        net[i] = (int *)calloc(g->v,sizeof(int));
    }
    net[0][0] = g->arr[1]->src;
    card[0]++;

    for(i = 2; i < g->v; i++){
        j = 0;
        while(j <= c){
            if(dist[net[j][0]][i]!=INF && net[j][0]!=0){
                net[j][card[j]] = i;
                card[j]++;
                j = c+1;
            }else if(net[j][0]==0){
                net[j][0] = i;
                card[j]++;
                j = c+1;
                c++;
            }
            j++;
        }
    }
    Networks = c;
    return net;

}

/** ------------------------------ FASE 2 (CHECK) ---------------------------- **/

int  **floydWarshall (int **graph, struct graph* g, struct graph* sr) {

    int i, j, k;
    int **dist = (int **)malloc(sizeof(int *)*g->v);
    int **net;
    struct node *temp;

    for(i = 0; i < g->v; i++){
        dist[i] = (int *)malloc(sizeof(int)*g->v);
        graph[i] = (int *)malloc(sizeof(int)*g->v);
    }


    for(i = 0;i < g->v; i++){
        for(j = 0; j < g->v; j++){
            if(i!=j){
                graph[i][j] = INF;
            }else{
                graph[i][j] = 0;
            }
        }
    }
    for(i = 1; i < g->v; i++){
        temp = g->arr[i];
        while(temp!=NULL){
            graph[i][temp->val] = temp->cost;
            temp = temp->next;
        }
    }

    for (i = 1; i < g->v; i++){
        for (j = 1; j < g->v; j++){
            dist[i][j] = graph[i][j];
            if(graph[i][j]==INF){
                graph[i][j]=0;
            }
        }
    }

    V = g->v;
    for (k = 1; k < g->v; k++) {
        for (i = 1; i < g->v; i++) {
            for (j = 1; j < g->v; j++) {
                if (dist[i][k] + dist[k][j] < dist[i][j])
                    dist[i][j] = dist[i][k] + dist[k][j];
            }
        }
    }


    net = countNetworks(dist,g);

    for(i = 0; i < (sr->v)-1 && Cable != -INF; i++){
        for(j = i+1; j < (sr->v) && Cable != -INF; j++){
                if(dist[sr->arr[i]->src][sr->arr[j]->src] != INF){
                    Cable += dist[sr->arr[i]->src][sr->arr[j]->src];
                }
        }

    }
    if(Cable == -INF){
        Cable = 0;
    }
    return net;
}

/** ------------------------------ FASE 3 ---------------------------- **/

void bellmanFord(struct graph* g, int src, struct graph* sr, int *ses){

    int i, j, z, u ,v, weight;
    int max = INF;
    struct node *temp;
    int *shortest = (int *)malloc(sizeof(int)*g->v);

    for(z = 0; z < sr->v; z++){

        src = sr->arr[z]->src;
        for (i = 0; i < g->v; i++){
            shortest[i] = INF;
        }
        shortest[src] = 0;


        for (i = 2; i <= g->v-1; i++) {
            for ( j = 1; j < g->v ; j++) {
                if(g->arr[j] != NULL){
                    temp = g->arr[j];
                    u = g->arr[j]->src;
                    while(temp != NULL){
                        v = temp->val;
                        weight = temp->cost;
                        if (shortest[u] != INF && shortest[u] + weight < shortest[v]){
                            shortest[v] = shortest[u] + weight;
                        }
                        temp = temp->next;
                    }
                    if(i == g->v-1){
                        if(ses[u]!=0){
                            if(shortest[u] < INF){
                                Topology += shortest[u];
                            }
                        }
                    }
                }
            }
        }
        if(max > Topology && Topology != 0){
            max = Topology;
        }
        Topology = 0;
    }
    if(max==INF){
        max = 0;
    }
    Topology = max;
    return;
}

/** --------------------------- INITIALIZER ----------------------------- **/


void init(){

    int size;
    int links;
    int u, v, c,i,j;

    char *token;
    char input[MAX];

    int *mst;
    int *server_ez;
    int **net;
    int **dist;

    struct graph* adj;
    struct graph* rej;
    struct graph* sr;

    scanf("%[^\n]s", input);

    while (strcmp("0", input) != 0) {
        token = strtok(input, "");
        size = atoi(token);

        /**Create Main structure with size edges**/
        adj = createGraph(size+1);
        NServers = 0;
        links = 0;
        while (strcmp("0", input) != 0) {
            scanf(" %[^\n]s", input);
            token = strtok(input, " ");
            u = atoi(token);
            if(u!=0){
                token = strtok(NULL, " ");
                v = atoi(token);
                token = strtok(NULL, " ");
                c = atoi(token);
                addEdge(adj,u,v,c);
            }
            links++;
        }

        /**Main operations have to work inside the cycle**/
        server_ez = (int *)calloc(adj->v,sizeof(int));
        sr = ArPoints(adj,server_ez);

        if(NServers){
            printf("%d ",NServers);
            if(NServers > 1){
                dist =( int **)malloc(sizeof(int *)*adj->v);
                /**It only is considered for an all-connected network**/
                net = floydWarshall(dist,adj,sr);
                if(Cable!=-INF){
                    printf("%d ",Cable);
                }else{
                    printf("0 ");
                }
                Cable = 0;

                /**TBA**/
                bellmanFord(adj,sr->arr[0]->src,sr,server_ez);
                if(Topology!=-INF){
                    printf("%d\n",Topology);
                }else{
                    printf("0\n");
                }
                Topology = 0;


            }else{
                printf("0 0\n");
            }

        }else{
            printf("no server\n");
        }
        if(rej!=NULL && net != NULL && j && i && mst);
        scanf(" %[^\n]s", input);
        free(sr);
        free(server_ez);
        free(adj);
    }

}

int main(){

    init();
    return 0;
}
