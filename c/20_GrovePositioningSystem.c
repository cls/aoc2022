#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define LEN(X) ((X)->type == Branch ? (X)->branch.len : 1)

struct node {
    enum { Branch, Leaf } type;
    union {
        struct {
            int pos;
            int len;
            struct node *left, *right;
        } branch;
        struct {
            long num;
            struct node *tail;
        } leaf;
    };
    struct node *parent;
};

static long decrypt(struct node *head, int rep);
static struct node *find_leaf(struct node *tree, int pos);
static void insert_leaf(struct node **treep, struct node *node, struct node *parent, int pos);
static struct node *remove_leaf(struct node **treep, int pos);
static int leaf_to_pos(struct node *node);
static void repair_branch(struct node **treep);
static void rotate_left(struct node **treep);
static void rotate_right(struct node **treep);

int
main(void)
{
    struct node *head = NULL;
    struct node **tailp = &head;

    for (int num, pos = 0; scanf("%d\n", &num) == 1; pos++) {
        struct node *node = malloc(sizeof *node);

        node->type = Leaf;
        node->leaf.num = num;

        *tailp = node;
        tailp = &node->leaf.tail;
    }

    *tailp = NULL;

    long part1 = decrypt(head, 1);

    printf("%ld\n", part1);

    for (struct node *node = head; node; node = node->leaf.tail) {
        node->leaf.num *= 811589153; // decryption key
    }

    long part2 = decrypt(head, 10);

    printf("%ld\n", part2);

    for (struct node *node = head; node; node = node->leaf.tail) {
        free(node);
    }

    return 0;
}

long
decrypt(struct node *head, int rep)
{
    struct node *tree = NULL;
    struct node *zero = NULL;
    int pos = 0;

    for (struct node *node = head; node; node = node->leaf.tail) {
        insert_leaf(&tree, node, NULL, pos++);

        if (node->leaf.num == 0) {
            assert(!zero);
            zero = node;
        }
    }

    assert(zero);

    for (int i = 0; i < rep; i++) {
        for (struct node *node = head; node; node = node->leaf.tail) {
            int pos = leaf_to_pos(node);

            struct node *removed_node = remove_leaf(&tree, pos);
            assert(removed_node == node);

            int new_pos = (pos + node->leaf.num) % tree->branch.len;
            if (new_pos < 0) {
                new_pos += tree->branch.len;
            }

            insert_leaf(&tree, node, NULL, new_pos);
        }
    }

    int zero_pos = leaf_to_pos(zero);

    struct node *a = find_leaf(tree, (zero_pos + 1000) % tree->branch.len);
    struct node *b = find_leaf(tree, (zero_pos + 2000) % tree->branch.len);
    struct node *c = find_leaf(tree, (zero_pos + 3000) % tree->branch.len);

    long sum = a->leaf.num + b->leaf.num + c->leaf.num;

    while (tree) {
        remove_leaf(&tree, 0);
    }

    return sum;
}

struct node *
find_leaf(struct node *tree, int pos)
{
    assert(tree);

    if (tree->type == Leaf) {
        return tree;
    } else if (pos < tree->branch.pos) {
        return find_leaf(tree->branch.left, pos);
    } else {
        return find_leaf(tree->branch.right, pos - tree->branch.pos);
    }
}

void
insert_leaf(struct node **treep, struct node *node, struct node *parent, int pos)
{
    if (!*treep) {
        *treep = node;
        node->parent = parent;
    } else {
        struct node *tree = *treep;

        if (tree->type == Leaf) {
            struct node *newtree = malloc(sizeof *newtree);

            newtree->type = Branch;
            newtree->branch.pos = 1;
            newtree->branch.len = 2;
            newtree->parent = parent;

            if (pos == 0) {
                newtree->branch.left = node;
                newtree->branch.right = tree;
            } else {
                newtree->branch.left = tree;
                newtree->branch.right = node;
            }

            node->parent = tree->parent = newtree;

            *treep = newtree;
        } else {
            tree->branch.len++;

            if (pos <= tree->branch.pos) {
                insert_leaf(&tree->branch.left, node, tree, pos);
                tree->branch.pos++;
                repair_branch(treep);
            } else {
                insert_leaf(&tree->branch.right, node, tree, pos - tree->branch.pos);
                repair_branch(treep);
            }
        }
    }
}

struct node *
remove_leaf(struct node **treep, int pos)
{
    assert(*treep);

    struct node *tree = *treep;
    struct node *node;

    if (tree->type == Leaf) {
        *treep = NULL;
        node = tree;
    } else {
        tree->branch.len--;

        if (pos < tree->branch.pos) {
            node = remove_leaf(&tree->branch.left, pos);
            tree->branch.pos--;
            if (tree->branch.left) {
                repair_branch(treep);
            } else {
                *treep = tree->branch.right;
                (*treep)->parent = tree->parent;
                free(tree);
            }
        } else {
            node = remove_leaf(&tree->branch.right, pos - tree->branch.pos);
            if (tree->branch.right) {
                repair_branch(treep);
            } else {
                *treep = tree->branch.left;
                (*treep)->parent = tree->parent;
                free(tree);
            }
        }
    }

    return node;
}

int
leaf_to_pos(struct node *node)
{
    int pos = 0;

    do {
        struct node *parent = node->parent;

        if (!parent) {
            break;
        }

        assert(parent->type == Branch);

        if (node != parent->branch.left) {
            assert(node == parent->branch.right);
            pos += parent->branch.pos;
        }

        node = parent;
    } while (node);

    return pos;
}

void
repair_branch(struct node **treep)
{
    struct node *tree = *treep;

    assert(tree->type == Branch);

    if (tree->branch.pos > tree->branch.len - tree->branch.pos + 1) {
        rotate_right(treep);
    } else if (tree->branch.pos < tree->branch.len - tree->branch.pos - 1) {
        rotate_left(treep);
    }
}

void
rotate_left(struct node **treep)
{
    struct node *tree = *treep;
    assert(tree->type == Branch);

    struct node *newtree = tree->branch.right;
    assert(newtree->type == Branch);

    tree->branch.right = newtree->branch.left;
    tree->branch.right->parent = tree;
    newtree->branch.left = tree;
    newtree->branch.pos = tree->branch.pos + LEN(tree->branch.right);
    newtree->branch.len = tree->branch.len;
    newtree->parent = tree->parent;
    // tree->branch.pos is unchanged
    tree->branch.len = newtree->branch.pos;
    tree->parent = newtree;
    *treep = newtree;
}

void
rotate_right(struct node **treep)
{
    struct node *tree = *treep;
    assert(tree->type == Branch);

    struct node *newtree = tree->branch.left;
    assert(newtree->type == Branch);

    tree->branch.left = newtree->branch.right;
    tree->branch.left->parent = tree;
    newtree->branch.right = tree;
    newtree->branch.pos = LEN(newtree->branch.left);
    newtree->branch.len = tree->branch.len;
    newtree->parent = tree->parent;
    tree->branch.pos = LEN(tree->branch.left);
    tree->branch.len = tree->branch.pos + LEN(tree->branch.right);
    tree->parent = newtree;
    *treep = newtree;
}
