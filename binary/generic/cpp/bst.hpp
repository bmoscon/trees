/*
 * bst.hpp
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

#include <iostream>
#include <queue>
#include <algorithm>


template <class K, class D>
class BSTree {
public:
  BSTree() : root(NULL) {}
  ~BSTree()
  {
    freeTree(root);
  }
  
  void insert(K &key, D &data)
  {
    if (!root) {
      root = new Node<K,D>(key, data);
      return;
    } 

    Node<K,D> *head = root;
    Node<K,D> *prev = NULL;

    while (head) {
      prev = head;
      if (key < head->key) {
	head = head->left;
      } else {
	head = head->right;
      }
    }

    if (key < prev->key) {
      prev->left = new Node<K,D>(key, data);
    } else {
      prev->right = new Node<K,D>(key, data);
    }
  }


  void inOrderTraversal() const
  {
    inOrder(root);
    std::cout << std::endl;
  }

  void preOrderTraversal() const
  {
    preOrder(root);
    std::cout << std::endl;
  }

  void postOrderTraversal() const
  {
    postOrder(root);
    std::cout << std::endl;
  }

  void levelOrderTraversal() const
  {
    levelOrder(root);
    std::cout << std::endl;
  }

  K leastCommonAncestor(const K &k1, const K &k2) const
  {
    if (!root || root->key == k1 || root->key == k2) {
      return (K());
    }

    return(LCA(root, k1, k2));
  }
    
  
private:
  template <class S, class T>
  struct Node {
    Node() : key(NULL), data(NULL), left(NULL), right(NULL) {}
    Node(const S &k, const T &d) : key(k), data(d), left(NULL), right(NULL) {}
    Node(const S &k, const T &d, Node<S,T> *l, Node<S,T> *r) : key(k), data(d), left(l), right(r) {}
    
    S key;
    T data;
    Node<S,T> *left;
    Node<S,T> *right;
  };

  
  void inOrder(const Node<K,D> *n) const
  {
    if (n) {
      inOrder(n->left);
      std::cout << n->key << ", ";
      inOrder(n->right);
    }
  }

  void preOrder(const Node<K,D> *n) const
  {
    if (n) {
      std::cout << n->key << ", ";
      preOrder(n->left);
      preOrder(n->right);
    }
  }

  void postOrder(const Node<K,D> *n) const
  {
    if (n) {
      postOrder(n->left);
      postOrder(n->right);
      std::cout << n->key << ", ";
    }
  }

  void levelOrder(const Node<K,D> *n) const
  {
    std::queue<const Node<K,D> *> q;
    q.push(n);

    while (!q.empty()) {
      n = q.front();
      q.pop();
      
      std::cout << n->key << ", ";
      if (n->left) {
	q.push(n->left);
      }
      
      if (n->right) {
	q.push(n->right);
      }
    }
  }

  K LCA(const Node<K,D> *n, const K &k1, const K &k2) const 
  {
    if (!n) {
      return (K());
    }

    if (std::max(k1, k2) < n->key) {
      return (LCA(n->left, k1, k2));
    } else if (std::min(k1, k2) > n->key) {
      return (LCA(n->right, k1, k2));
    } else {
      return (n->key);
    }
  }

  void freeTree(Node<K,D> *n)
  {
    if (n) {
      freeTree(n->left);
      freeTree(n->right);
      delete n;
    }
  }
  
  
  Node<K,D> *root;

};

