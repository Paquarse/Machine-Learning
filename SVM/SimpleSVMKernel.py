
# coding: utf-8

# # SVM Lineaire et non-lineaire
# Application sur les donnees jouet (voir Hastie et Tibshrani)

from __future__ import print_function
from __future__ import division
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

import scipy.io as sio
from sklearn.metrics import accuracy_score, roc_curve
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

from sklearn.svm import SVC

plt.close("all")


#%% Trace frontiere decision et marge
def plot_decision_margin_2d(X, y, classifier, resolution=0.02, titre=' '):

    # setup marker generator and color map
    markers = ('s', 'v', 'o', '^', 'x')
    colors = ('red', 'blue', 'lightgreen', 'gray', 'cyan')
    cmap = ListedColormap(colors[:len(np.unique(y))])

    # plot the decision surface
    x1_min, x1_max = X[:, 0].min() - 0, X[:, 0].max() + 0
    x2_min, x2_max = X[:, 1].min() - 0, X[:, 1].max() + 0
    xx1, xx2 = np.meshgrid(np.arange(x1_min, x1_max, resolution),
                         np.arange(x2_min, x2_max, resolution))
    Z = classifier.predict(np.array([xx1.ravel(), xx2.ravel()]).T)
    Z = Z.reshape(xx1.shape)
    plt.figure()
    plt.contourf(xx1, xx2, Z, alpha=0.4, cmap=cmap)
    plt.xlim(xx1.min(), xx1.max())
    plt.ylim(xx2.min(), xx2.max())

    # margin
    Z = classifier.decision_function(np.array([xx1.ravel(), xx2.ravel()]).T)
    Z = Z.reshape(xx1.shape)
    cs = plt.contour(xx1, xx2, Z, levels=[-1, 0, 1], colors=['r', 'g', 'b'], linewidths=2.5)
    plt.clabel(cs)
    
    # plot class samples
    for idx, cl in enumerate(np.unique(y)):
        plt.scatter(x=X[y == cl, 0], y=X[y == cl, 1],
                    alpha=0.45, c=cmap(idx),
                    marker=markers[idx], label= 'classe {}'.format(cl))
    plt.legend(loc='best')
    plt.title(titre, fontsize=12)
    
#%% Dataset : Mixture of gaussian (disponible sur Moodle)
# Donnees apprentissage
data_a = sio.loadmat('./mixtureexampleTRAIN.mat')
Xa, Ya = data_a['Xa'], data_a['Ya'][:,0]
print('\nCaractéristiques jeu apprentissage : ')
print('Nombre de points : {}'.format(Xa.shape[0]))
print('Nombre de variables : {}'.format(Xa.shape[1]))
print('Nombre de classes : {}'.format(len(np.unique(Ya))))
classes, nbpoints = np.unique(Ya, return_counts=True)
for i, lab in enumerate(classes):
    print('Classe {} comprend {} points'.format(lab, nbpoints[i]))


# Donnees test
data_t = sio.loadmat('./mixtureexampleTEST.mat')
Xt, Yt = data_t['Xt'], data_t['Yt'][:,0]
print('\nCaractéristiques jeu de test : ')
classes, nbpoints = np.unique(Yt, return_counts=True)
for i, lab in enumerate(classes):
    print('Classe {} comprend {} points'.format(lab, nbpoints[i]))

#%% Decoupage des donnees app en jeu de validation et app
Xa, Xv, Ya, Yv = train_test_split(Xa, Ya, shuffle=True, test_size=0.5, stratify=Ya)


#%% Normalisation
sc = StandardScaler(with_mean=True, with_std=True)
sc = sc.fit(Xa)
Xa = sc.transform(Xa)
Xv = sc.transform(Xv)
Xt = sc.transform(Xt)


#%% 
# definition du modele SVM Lineaire
paramC = 1
paramKer = 1
clf_ker = SVC(kernel='rbf', C = paramC, gamma=paramKer)
# apprentissage des parametres du SVM Lineaire sur le jeu d'apprentissage
clf_ker.fit(Xa, Ya)

#%% Trace de la frontiere de decision et de la marge 
plot_decision_margin_2d(Xa, Ya, clf_ker, 0.02, titre='{} avec C = {}'.format("SVM kernel", paramC))


#%% Erreur de classification en test du SVM Lineaire obtenu
err_app = 1 - accuracy_score(Ya, clf_ker.predict(Xa))
print('\nSVM kernel : erreur apprentissage = {}'.format(err_app))
err_test = 1 - accuracy_score(Yt, clf_ker.predict(Xt))
print('SVM kernel : erreur test = {}'.format(err_test))
