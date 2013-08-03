/*
 * bst.c
 *
 * Generic Binary Search Tree
 *
 *
 *
 * Copyright (C) 2013  Bryant Moscon - bmoscon@gmail.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to 
 * deal in the Software without restriction, including without limitation the 
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * 1. Redistributions of source code must retain the above copyright notice, 
 *    this list of conditions, and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 *    this list of conditions and the following disclaimer in the documentation 
 *    and/or other materials provided with the distribution, and in the same 
 *    place and form as other copyright, license and disclaimer information.
 *
 * 3. The end-user documentation included with the redistribution, if any, must 
 *    include the following acknowledgment: "This product includes software 
 *    developed by Bryant Moscon (http://www.bryantmoscon.org/)", in the same 
 *    place and form as other third-party acknowledgments. Alternately, this 
 *    acknowledgment may appear in the software itself, in the same form and 
 *    location as other such third-party acknowledgments.
 *
 * 4. Except as contained in this notice, the name of the author, Bryant Moscon,
 *    shall not be used in advertising or otherwise to promote the sale, use or 
 *    other dealings in this Software without prior written authorization from 
 *    the author.
 *
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
 * THE SOFTWARE.
 *
 */

#include "bst.h"


static inline void node_init(node_t *n, int d, int k)
{
  n->left = NULL;
  n->right = NULL;
  n->d = d;
  n->k = k;
}

node_t* tree_insert(node_t *root, int data, int key)
{
  node_t *t;
  node_t *p;
  
  if (root == NULL) {
    t = malloc(sizeof(node_t));

    if (!t) {
      // throw some error
      return (NULL);
    }
    
    node_init(t, data, key);
    
    return (t);
  }
  
  t = root;

  do {
    p = t;
    if (key < t->k) {
      t = t->left;
    } else {
      t = t->right;
    }
  } while (t != NULL);

  if (key < p->k) {
    p->left = malloc(sizeof(node_t));
    if (!p->left) {
      // error
      return (NULL);
    }
    node_init(p->left, data, key);
  } else {
    p->right = malloc(sizeof(node_t));
    if (!p->right) {
      // error
      return (NULL);
    }
    node_init(p->right, data, key);
  }

  return (root);
}

void tree_free(node_t *n)
{
  if (!n) {
    return;
  } 

  tree_free(n->left);
  tree_free(n->right);
  free(n);
}

void tree_in_order(node_t *n, void (*fp)(int, int))
  {
    if (n) {
      tree_in_order(n->left, fp);
      fp(n->k, n->d);
      tree_in_order(n->right, fp);
    }
  }






