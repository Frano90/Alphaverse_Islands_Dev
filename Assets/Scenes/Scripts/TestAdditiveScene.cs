using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class TestAdditiveScene : MonoBehaviour
{
    [SerializeField] int[] scenesToLoad = new int[0];


    private void Start()
    {
        LoadScenesLikeABaus();
    }

    void LoadScenesLikeABaus()
    {
        StartCoroutine(LoadSceneAsync());
    }

    IEnumerator LoadSceneAsync()
    {
        for (int i = 0; i < scenesToLoad.Length; i++)
        {
            var async = SceneManager.LoadSceneAsync(scenesToLoad[i], LoadSceneMode.Additive);
            while(async.progress < 0.9)
            {
                yield return new WaitForEndOfFrame();
            }
        }
    }
}
