using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[ExecuteInEditMode]
public class LD_DesableComponent : MonoBehaviour
{
    public enum ComponentType
    {
        MeshRender,
        Collider,
        RigiBody,
        Mesh,
        Animator,
        NaveMeshAgent
    }
    public enum state
    {
        deactive,
        active,
        reset,
        neutral
    }
    public state stateMethod;
  

    public ComponentType type;
   
    // Update is called once per frame
    void Update()
    {
        switch (stateMethod)
        {
            case state.deactive:
                DeactiveSelected();
                stateMethod = state.neutral;
                break;
            case state.active:
                ActiveSelected();
                stateMethod = state.neutral;
                break;
            case state.reset:
                break;
            case state.neutral:
                break;
            default:
                break;
        }
      
       
    }


    void DeactiveSelected()
    {
        switch (type)
        {
            case ComponentType.MeshRender:
                // List<MeshRenderer> virtualList = new List<MeshRenderer>();
                MeshRenderer[] virtualList = this.transform.GetComponentsInChildren<MeshRenderer>();
                virtualList = transform.GetComponentsInChildren<MeshRenderer>();
                foreach (MeshRenderer v in virtualList)
                {
                    v.enabled = false;
                }
                break;
            case ComponentType.Collider:
                Collider[] _Collider = this.transform.GetComponentsInChildren<Collider>();
                _Collider = transform.GetComponentsInChildren<Collider>();
                foreach (Collider v in _Collider)
                {
                    v.enabled = false;
                }
                break;
            case ComponentType.RigiBody:
                break;
            case ComponentType.Mesh:
              
                break;
            case ComponentType.Animator:
                break;
            case ComponentType.NaveMeshAgent:
                break;
            default:
                break;
        }
    }
    void ActiveSelected()
    {
        switch (type)
        {
            case ComponentType.MeshRender:
                // List<MeshRenderer> virtualList = new List<MeshRenderer>();
                MeshRenderer[] virtualList = this.transform.GetComponentsInChildren<MeshRenderer>();
                virtualList = transform.GetComponentsInChildren<MeshRenderer>();
                foreach (MeshRenderer v in virtualList)
                {
                    v.enabled = true;
                }
                break;
            case ComponentType.Collider:
                Collider[] _Collider = this.transform.GetComponentsInChildren<Collider>();
                _Collider = transform.GetComponentsInChildren<Collider>();
                foreach (Collider v in _Collider)
                {
                    v.enabled = true;
                }
                break;
            case ComponentType.RigiBody:
                break;
            case ComponentType.Mesh:
                break;
            case ComponentType.Animator:
                break;
            case ComponentType.NaveMeshAgent:
                break;
            default:
                break;
        }
    }
}
