#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define MAX 1024
#define INF 999999

int V = 0;
int time = 0;
int NServers = 0;
int Cable = 0;
int Topology = 0;

struct node{

    int src;
    int val;
    int cost;
    struct node* next;

};

struct graph{

    int v;
    struct node** arr;

};

int min(int a,int b){
    return(a<b?a:b);
}

void printServers(struct graph *s){

    int i = 0;
    struct node *temp;
    for(;i<s->v;i++){
        printf("___SERVER %d: ",s->arr[i]->src);
        temp = s->arr[i];
        while(temp!=NULL){
            printf("%d(%d) ",temp->val,temp->cost);
            temp = temp->next;
        }
        printf("\n");
    }

}

void printArr(int dist[], int n){
    int i;
    printf("Vertex   Distance from Source\n");
    for ( i = 0; i < n; ++i)
        printf("%d \t\t %d\n", i, dist[i]);
}

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

void floydWarshall (struct graph* g, struct graph* sr) {

    int **dist = (int **)malloc(sizeof(int *)*g->v);
    int **graph = (int **)malloc(sizeof(int *)*g->v);
    struct node *temp;
    int i, j, k;

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

    for(i = 0; i < (sr->v) && Cable != -INF; i++){
        if(i!=sr->v-1){
            if(dist[sr->arr[i]->src][sr->arr[i+1]->src] != INF){
                Cable += dist[sr->arr[i]->src][sr->arr[i+1]->src];
            }else{
                Cable = -INF;
            }
        }

    }
    if(Cable == -INF){
        Cable = 0;
    }else{
        if(sr->v > 2){
            Cable += dist[sr->arr[0]->src][sr->arr[(sr->v)-1]->src];
        }
    }


}

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
    //printArr(dist, g->v);

    return;
}

void init(){

    int size;
    int links;
    int u, v, c;
    char *token;
    int *server_ez;
    char input[MAX];
    struct graph* adj;

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

        if(sr!=NULL);

        if(NServers){
            printf("%d ",NServers);
            if(NServers > 1){

                /**It only is considered for an all-connected network**/
                floydWarshall(adj,sr);
                if(Cable!=-INF){
                    printf("%d ",Cable);
                }else{
                    printf("0 ");
                }
                Cable = 0;

                /**Bellman-Ford**/
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

        scanf(" %[^\n]s", input);

        free(server_ez);
        free(sr);
        free(adj);
    }

}

int main(){

    init();
    return 0;
}
