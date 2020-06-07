#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define MAX 128
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

struct subset{
    int parent;
    int rank;
};


/** ------------------------------ MISC (PRINTS AND UTIL) ---------------------------- **/

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


/** ------------------------------ FASE 0 (GRAPH UTIL) ---------------------------- **/


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

/** -------------------------------FASE 1 (SERVERS) ---------------------------- **/


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

/** ------------------------------ FASE 2 (FULLY CONNECTED) ---------------------------- **/

int  **floydWarshall(struct graph* g, struct graph* sr) {

    int i, j, k;
    int **dist = (int **)malloc(sizeof(int *)*g->v);
    int **graph = (int **)malloc(sizeof(int *)*g->v);
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


    /*printSolution(dist,g);*/
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
    return dist;
}

/** ------------------------------ FASE 3 (MINIMUM SPANNING TREE IN NETWORK) ---------------------------- **/

void swap(struct node* a, struct node* b){
    struct node t = *a;
    *a = *b;
    *b = t;
}

int subset(struct node **arr, int low, int high){

    int j;
    struct node *pivot = arr[high];
    int i = (low - 1);

    for (j = low; j <= high- 1; j++){

        if (arr[j]->cost < pivot->cost){
            i++;
            swap(arr[i], arr[j]);
        }

    }
    swap(arr[i + 1], arr[high]);
    return (i + 1);
}

void quickSort(struct node **arr, int low, int high){

    if (low < high){
        int pi = subset(arr, low, high);
        quickSort(arr, low, pi - 1);
        quickSort(arr, pi + 1, high);
    }
}

int find(struct subset subsets[], int i){

    if (subsets[i].parent != i)
        subsets[i].parent = find(subsets, subsets[i].parent);

    return subsets[i].parent;

}

void Union(struct subset subsets[], int x, int y){

    int xroot = find(subsets, x);
    int yroot = find(subsets, y);

    if (subsets[xroot].rank < subsets[yroot].rank)
        subsets[xroot].parent = yroot;
    else if (subsets[xroot].rank > subsets[yroot].rank)
        subsets[yroot].parent = xroot;

    else
    {
        subsets[yroot].parent = xroot;
        subsets[xroot].rank++;
    }
}

int myComp(const void* a, const void* b){

    struct node* a1 = (struct node*)a;
    struct node* b1 = (struct node*)b;
    return a1->cost > b1->cost;
}

void primMST(struct graph* graph, int V){

    int v,x,y;
    int v_max = V;
    int i = 0;
    int j = 0;
    struct node *next_edge;
    struct subset *subsets;
    struct node **result;

    if(V < graph->v){
        v_max = graph->v;
    }

    subsets =(struct subset*) malloc( v_max * sizeof(struct subset) );
    result = (struct node **)malloc( v_max * sizeof(struct node *));

    for (v = 0; v < v_max; v++){
        subsets[v].parent = v;
        subsets[v].rank = 0;
    }

    while (j < graph->v - 1 && i < graph->v){
        next_edge = graph->arr[i++];
        x = find(subsets, next_edge->src);
        y = find(subsets, next_edge->val);
        if (x != y){
            result[j] = (struct node*) malloc(sizeof(struct node));
            result[j++] = next_edge;
            Union(subsets, x, y);
        }

    }

    for (i = 0; i < j; ++i){
        if(result[i]->cost!=INF){
            Topology+=result[i]->cost;
        }
    }
    return;
}

struct graph *transform(struct graph *sr, int v, int **dist){

    int i,k,j;
    struct node* novo;
    struct graph *prims = createGraph(256);
    j = 0;
    for(i = 0; i < sr->v - 1; i++){
        for(k = i+1 ;k < sr->v; k++){
            novo = (struct node*)malloc(sizeof(struct node));
            novo->src = sr->arr[i]->src;
            novo->val = sr->arr[k]->src;
            novo->cost = dist[sr->arr[i]->src][sr->arr[k]->src];
            prims->arr[j] = novo;
            j++;
        }
    }
    prims->v = j;
    quickSort(prims->arr,0,prims->v-1);
    return prims;
}

/** --------------------------- [INITIALIZER] ----------------------------- **/

void init(){

    int size;
    int links;
    int u, v, c;

    char *token;
    char input[MAX];

    int *server_ez;
    int **dist;

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

        if(NServers){
            printf("%d ",NServers);

            if(NServers > 1){


                /**It only is considered for an all-connected network**/
                dist = floydWarshall(adj,sr);

                if(Cable!=-INF){
                    printf("%d ",Cable);
                }else{
                    printf("0 ");
                }
                Cable = 0;
                /** End of FW **/

                /**Prim**/
                if(sr->v > 2){
                    sr = transform(sr,adj->v,dist);
                    primMST(sr,adj->v);
                }else{
                    if(dist[sr->arr[0]->src][sr->arr[1]->src]!=INF){
                        Topology = dist[sr->arr[0]->src][sr->arr[1]->src];
                    }
                }

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

        free(sr);
        free(server_ez);
        free(adj);
    }

}

int main(){

    init();
    return 0;
}
